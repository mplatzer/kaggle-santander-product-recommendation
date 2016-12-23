library(data.table) # use data.table for speed & memory efficiency
library(stringr)    # use stringr for string manipulation

# define `relevant` products; leave those out which have had only very few activations
products <- data.table(product = c(
  "ind_cco_fin_ult1", "ind_cno_fin_ult1", "ind_ctma_fin_ult1",
  "ind_ctop_fin_ult1", "ind_ctpp_fin_ult1", "ind_dela_fin_ult1",
  "ind_ecue_fin_ult1", "ind_fond_fin_ult1", "ind_nom_pens_ult1",
  "ind_nomina_ult1", "ind_reca_fin_ult1", "ind_recibo_ult1",
  "ind_tjcr_fin_ult1", "ind_valo_fin_ult1"))[, product_id := .I - 1]

# pre-process input data (if it hasn't been done yet)
# 1. stack train & test data
# 2. fill missing data, and fix suspicious one
# 3. add lags
# 4. convert target to activations
wide.fn <- "wide.csv"
if (file.exists(wide.fn)) {
  wide <- fread(wide.fn, stringsAsFactors = TRUE)
} else {
  if (!file.exists("test_ver2.csv"))
    stop("please download `test_ver2.csv.zip` and `test_ver2.csv.zip` from ",
         "https://www.kaggle.com/c/santander-product-recommendation/data and unzip ",
         "to your working directory")
  # read in train & test data
  train_raw <- fread("train_ver2.csv")
  test_raw <- fread("test_ver2.csv")
  # drop unused columns
  drop_cols <- c("tipodom", "ult_fec_cli_1t", "cod_prov")
  drop_prods <- setdiff(grep("^ind.*ult1", names(train_raw), value=T), products$product)
  train_raw[, (c(drop_cols, drop_prods)) := NULL]
  test_raw[, (drop_cols) := NULL]
  # stack train & test data together
  wide <- rbind(train_raw, test_raw, fill = TRUE, use.names = TRUE)
  # Note: memory usage peaks here at 5.9 GB
  rm(train_raw, test_raw)
  # convert integers to numerics
  int_cols <- c("age", "renta", "antiguedad", "ind_nuevo", "indrel", "ind_actividad_cliente")
  wide[, (int_cols) := lapply(.SD, as.numeric), .SDcols=int_cols]
  # fix age; fill missing with overall mean(age)
  wide[age < 20 | age > 99, age := NA]
  wide[is.na(age), age := mean(wide$age, na.rm = TRUE)]
  # fix renta; fill missing with province specific mean(renta)
  vs <- wide[!is.na(renta), .(v=mean(renta)), by=nomprov]
  setkey(vs, nomprov)
  wide[is.na(renta), renta := vs[nomprov]$v]
  # fix antiguedad; fill missing with overall mean(antiguedad)
  wide[antiguedad < 0, antiguedad := NA]
  wide[is.na(antiguedad), antiguedad := mean(wide$antiguedad, na.rm = TRUE)]
  # fill missing ind_nuevo, indrel, ind_actividad_cliente, indrel_1mes
  wide[is.na(ind_nuevo), ind_nuevo := mean(wide$ind_nuevo, na.rm = TRUE)]
  wide[is.na(indrel), indrel := mean(wide$indrel, na.rm = TRUE)]
  wide[is.na(ind_actividad_cliente), ind_actividad_cliente := mean(wide$ind_actividad_cliente, na.rm = TRUE)]
  wide[is.na(indrel_1mes), indrel_1mes := ""]
  # simplify country to `ES` and `other`
  wide[, pais_residencia := ifelse(pais_residencia %in% c("ES", ""), "ES", "other")]
  # fill missing product status with 0
  for (p in products$product) {
    set(wide, i = which(wide[["fecha_dato"]] != "2016-06-28" & is.na(wide[[p]])), j = p, value = 0L)
  }
  # define helper method to add lags
  add_lag <- function(cols, shift) {
    # note: this approach could be probably made faster
    months <- c(sort(unique(wide$fecha_dato)), "2016-07", "2016-08", "2016-09", "2016-10", "2016-11")
    px <- wide[, c("ncodpers", "fecha_dato", cols), with=F]
    px[, fecha_dato := months[match(fecha_dato, months) + shift]]
    cols_new <- paste0("p", shift, "_", cols)
    setnames(px, cols, cols_new)
    setkey(px, ncodpers, fecha_dato)
    setkey(wide, ncodpers, fecha_dato)
    cat("adding", paste0(cols_new, collapse=", "), "\n")
    wide <- merge(wide, px, by = c("ncodpers", "fecha_dato"), all.x = TRUE)
    for (i in seq_along(cols)) {
      wide[is.na(get(cols_new[i])), (cols_new[i]) := get(cols[i])]
    }
    wide
  }
  # add some lags for customer covariates
  wide <- add_lag("renta", 1)
  wide <- add_lag("renta", 3)
  wide[, p3_renta_ratio := renta / p3_renta]
  wide[, p3_renta := renta - p3_renta]
  wide[, p1_renta_ratio := renta / p1_renta]
  wide[, p1_renta := renta - p1_renta]
  wide <- add_lag("ind_actividad_cliente", 1)
  wide <- add_lag("ind_actividad_cliente", 2)
  wide <- add_lag("segmento", 1)
  # add lags 1 to 5 for product status
  wide <- add_lag(products$product, 1)
  wide <- add_lag(products$product, 2)
  wide <- add_lag(products$product, 3)
  wide <- add_lag(products$product, 4)
  wide <- add_lag(products$product, 5)
  # add whether product was active at all in p2/p3/p4/p5
  # add number of activations during p1/p2/p3/p4/p5
  for (p in products$product) {
    cat(p, "\n")
    ps <- paste0("p", 1:5, "_", p)
    wide[, (paste0("pmax_", p)) := pmax(get(ps[2]), get(ps[3]), get(ps[4]), get(ps[5]))]
    wide[, (paste0("pdiff_", p)) :=
           abs(pmax(0, get(ps[1]) - get(ps[2]))) +
           abs(pmax(0, get(ps[2]) - get(ps[3]))) +
           abs(pmax(0, get(ps[3]) - get(ps[4]))) +
           abs(pmax(0, get(ps[4]) - get(ps[5])))]
  }
  # convert product columns from state to activation flags
  idx <- which(wide$fecha_dato != "2016-06-28")
  for (p in products$product) {
    cat(p, "\n")
    wide[idx, (p) := pmax(get(p) - get(paste0("p1_", p)), 0L)]
  }
  # convert character columns to factors
  char_cols <- names(which(sapply(wide, is.character)))
  wide[, (char_cols) := lapply(.SD, as.factor), .SDcols = char_cols]
  # write to disk (4.7GB in file size)
  fwrite(wide, "wide.csv")
}

# helper method to convert from wide to tall format, with one (product x customer x month) per line
# only keep those lines, where customer has been inactive in previous period
wide2tall <- function(dt) {
  inactive <- melt(dt, id.vars = c("ncodpers", "fecha_dato"), measure.vars = paste0("p1_", products$product))[value==0, .(ncodpers, fecha_dato, variable)][, variable := str_sub(variable, 4)]
  tall <- melt(dt, measure.vars = products$product)
  setkey(inactive, ncodpers, fecha_dato, variable)
  setkey(tall, ncodpers, fecha_dato, variable)
  tall <- merge(inactive, tall, by=c("ncodpers", "fecha_dato", "variable"))
  setcolorder(tall, c("value", setdiff(names(tall), "value")))
  tall
}

# helper method to convert categorical variables to numeric values, starting with 0; this is required by LightGBM
fac2num <- function(dt) {
  factor_cols <- names(which(sapply(dt, class)=="factor"))
  dt[, (factor_cols) := lapply(.SD, function(x) as.integer(x) - 1), .SDcols = factor_cols]
}

# write data file for scoring
score <- wide[fecha_dato == "2016-06-28"]
score <- wide2tall(score)
score <- fac2num(score)
fwrite(score, "score.csv")

# drop customers from training set which haven't activated anything in a given month
act <- melt(wide, id.vars=c("ncodpers", "fecha_dato"), measure.vars=products$product)
act <- act[fecha_dato!='2016-06-28', .(is_active=any(value==1)), by=c('ncodpers', 'fecha_dato')][is_active==1]
setkey(act, ncodpers, fecha_dato)
setkey(wide, ncodpers, fecha_dato)
wide <- merge(act, wide)
wide[, is_active := NULL]
# Note: only 3.4068% of customers activate a product in a given month

# restrict training to Jun'15 for seasonal products,
# and use last 3 months for all others as training
train_months <- c("2015-06-28", "2016-03-28", "2016-04-28", "2016-05-28")
wide <- wide[fecha_dato %in% train_months]
tall <- wide2tall(wide)
seasonal_products <- c("ind_cco_fin_ult1", "ind_reca_fin_ult1")
tall <- tall[(fecha_dato=="2015-06-28" & variable %in% seasonal_products) |
               (fecha_dato!="2015-06-28" & !variable %in% seasonal_products)]
tall <- fac2num(tall)

# use 50k of the 894k records for validation
set.seed(123)
test.idx <- sample(1:nrow(tall), 50000)
train.idx <- setdiff(1:nrow(tall), test.idx)
fwrite(tall[train.idx], "train.csv")
fwrite(tall[test.idx], "test.csv")

# call LightGBM for training the model
system("~/LightGBM/lightgbm config=train.conf")

# call LightGBM for scoring the model
system("~/LightGBM/lightgbm config=predict.conf")

score[, score := fread("LightGBM_predict_result.txt")$V1]
sub <- score[, .(ncodpers, variable, score)]
setkey(sub, variable)
setkey(products, product_id)
sub <- merge(sub, products, by.x="variable", by.y="product_id", all.x = TRUE)

# leverage leaderboard probes
# figures thanks to Panos https://www.kaggle.com/c/santander-product-recommendation/forums/t/25727/question-about-map-7?forumMessageId=146330#post146330
probes <- c("ind_cco_fin_ult1" = 0.0096681,
            "ind_cder_fin_ult1" = 0.0000090,
            "ind_cno_fin_ult1" = 0.0017839,
            "ind_ctju_fin_ult1" = 0.0000502,
            "ind_ctma_fin_ult1" = 0.0004488,
            "ind_ctop_fin_ult1" = 0.0001949,
            "ind_ctpp_fin_ult1" = 0.0001142,
            "ind_deco_fin_ult1" = 0.0000000,
            "ind_dela_fin_ult1" = 0.0001142, #?
            "ind_deme_fin_ult1" = 0.0000000,
            "ind_ecue_fin_ult1" = 0.0019961,
            "ind_fond_fin_ult1" = 0.0001040,
            "ind_hip_fin_ult1" = 0.0000161,
            "ind_nom_pens_ult1" = 0.0021801,
            "ind_nomina_ult1" = 0.0021478,
            "ind_plan_fin_ult1" = 0.0000126,
            "ind_pres_fin_ult1" = 0.0000054,
            "ind_reca_fin_ult1" =  0.0032092,
            "ind_recibo_ult1" = 0.0086845,
            "ind_tjcr_fin_ult1" = 0.0041178,
            "ind_valo_fin_ult1" = 0.0002780,
            "ind_viv_fin_ult1" = 0.0000000)
probes <- data.table(product=names(probes), map7=unname(probes))
setkey(probes, product)
# rescale map7 to probabilities based on historic data
map7ratios <- function(month) {
  dd <- melt(wide[fecha_dato == month, c("ncodpers", products$product), with=F], id.vars='ncodpers')
  dd <- merge(dd, dd[, .(cnt=sum(value==1)), by=ncodpers], by='ncodpers', all.x=T)
  pr <- dd[, .(prob = mean(value)), by=variable]
  dx <- dd[,.(N = sum(value==1)), keyby = c("variable", "cnt")]
  dx[, T := sum(N), by=variable]
  dx[, M := ifelse(cnt==0, 0, N / (uniqueN(dd$ncodpers)*cnt))]
  dx <- merge(dx[, .(map7 = sum(M)), keyby=variable], pr, by="variable")
  dx[, ratio := map7 / prob]
  dx[, .(product = variable, ratio)]
}
probes <- merge(probes,
                rbind(map7ratios('2016-05-28')[!product %in% seasonal_products],
                      map7ratios('2015-06-28')[product %in% seasonal_products]),
                by = "product")
probes[, holdout := map7 / ratio]
probes <- probes[, .(product, holdout)]
# adjust predicted scores by ratio of activations in holdout set vs. activations in training set
corr <- merge(tall[train.idx, .(train = mean(value)),by=.(product_id=variable)], products, by="product_id")
corr <- merge(corr, probes, by="product")
corr[, ratio := holdout / train]
sub <- merge(sub, corr, by="product")
sub[, score := score * ratio]
# deactivate dela
sub[product=='ind_dela_fin_ult1', score := 0]

# write out submission format
setorder(sub, ncodpers, -score)
sub_wide <- sub[, .(added_products = paste0(head(product, 7), collapse=" ")), by = ncodpers]
fwrite(sub_wide, "submission.csv")
