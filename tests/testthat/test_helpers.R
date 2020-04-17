library(cvms)
context("helpers")

test_that("Helper count_nulls_in_list() works", {

  # skip_test_if_old_R_version()

  expect_equal(count_nulls_in_list(list(
    "cat" = NULL,
    "dog" = 3,
    "hat" = NULL
  )), 2)
})

test_that("Helper count_convergence_warnings() works", {
  expect_equal(count_convergence_warnings(c("Yes", "No", "No", "Yes", "No")), 3)
  expect_error(
    xpectr::strip_msg(count_convergence_warnings(c(
      "Yes", "No", "No", "Yes", "No", "Lol", "Nay"
    ))),
    xpectr::strip(paste0("1 assertions failed:\n * Variable 'unique(convergences)': M",
                         "ust be a subset of set {Yes,No}.")),
    fixed = T
  )
})

test_that("capture_fn()", {
  expect_equal(
    substr(capture_fn(
      function(x,y,z){x+y+z}
    ), 1, 22),
    "function(x,y,z){x+y+z}"
  )
  expect_equal(
    capture_fn(NULL),
    "NULL"
  )
})

test_that("nnet gives same predictions on mac and ubuntu", {
  testthat::skip("mac and ubuntu give different warnings")
  # Tested on both platforms on travis as well
  # Local test should run on mac as is

  xpectr::set_test_seed(10)

  dat <- participant.scores %>%
    dplyr::mutate(diagnosis = as.factor(diagnosis))

  # binomial
  predictions <- plyr::llply(1:10, function(s) {
    xpectr::set_test_seed(s)
    co <- testthat::capture_output(nn <- nnet::nnet(as.formula("diagnosis~score"), data = dat, size = 10))
    as.vector(predict(nn, dat))
  }) %>% unlist()

  expect_equal(
    predictions,
    c(
      1, 0.74558685723907, 0.578586438198845, 0.74558685723907, 0.580615724626656,
      0, 0.998890098306022, 0.616111813126343, 0.580615724626656, 0.588197506760497,
      0.578045175602885, 0, 0.74558685723907, 0.577916799755878, 0.57632507163764,
      0.99972789041577, 0.711360791916158, 0.616111813126343, 0.999998329721127,
      0.588197506760497, 0.579973146331912, 0.996241553095139, 0.60058844287862,
      0.578809802264348, 0.595351004005742, 0.577937467580419, 0.000202134102871739,
      0.627360442993519, 0.577900935866149, 0, 1, 0.666721786798279,
      0.611104734221879, 0.666721786798279, 0.611104734221879, 0, 1,
      0.611104734221879, 0.611104734221879, 0.611104734221879, 0.611104734221879,
      0, 0.666721786798279, 0.611104734221879, 0.611031240769425, 1,
      0.611127805162521, 0.611104734221879, 1, 0.611104734221879, 0.611104734221879,
      1, 0.611104734221879, 0.611104734221879, 0.611104734221879, 0.611104734221879,
      1.00856566789005e-05, 0.611104734221879, 0.611104734221879, 0,
      1, 0.764463434592399, 0.494841610269604, 0.764463434592399, 0.566127883565247,
      0.215408049162467, 1, 0.697774137967905, 0.566127883565247, 0.63456700711862,
      0.423544643076406, 0.126224173866663, 0.764463434592399, 0.368429302533936,
      0.268533642407709, 1, 0.754107188656388, 0.697774137967905, 1,
      0.63456700711862, 0.552011666457357, 1, 0.673242845547333, 0.509193734651035,
      0.660588208260785, 0.381962238281882, 0.225403261168915, 0.70962847024623,
      0.355092349714316, 0.108073623307058, 1, 0.724113674351695, 0.58282442741649,
      0.724113674351695, 0.59931877687094, 0.0821341520422677, 1, 0.649126815128951,
      0.59931877687094, 0.619006346983197, 0.563107486791626, 1.18430238270543e-05,
      0.724113674351695, 0.535752571789324, 0.34487721608998, 1, 0.706476255414935,
      0.649126815128951, 1, 0.619006346983197, 0.595937853833416, 1,
      0.635105282576637, 0.586130895190753, 0.629201817300479, 0.544401190406166,
      0.128336806782347, 0.65756914221701, 0.525292292028433, 2.32452222685714e-06,
      1, 0.765230535670973, 0.497113483318073, 0.765230535670973, 0.56818606704355,
      0.216425918368818, 1, 0.698899580616249, 0.56818606704355, 0.636220787357355,
      0.425807268658682, 0.126323836555748, 0.765230535670973, 0.370531320315304,
      0.270022823007995, 1, 0.75474192951717, 0.698899580616249, 1,
      0.636220787357355, 0.554129485298243, 1, 0.674589391993337, 0.511440303014397,
      0.662041083917444, 0.384115902502773, 0.226516089464678, 0.710640452968085,
      0.357135834740305, 0.10798886724238, 1, 0.720586816275157, 0.571629206675288,
      0.720586816275157, 0.589375265699162, 0, 1, 0.654013450672579,
      0.589375265699162, 0.615695751684985, 0.559778791833181, 0, 0.720586816275157,
      0.55323271339913, 0.545136939255642, 1, 0.707788414362377, 0.654013450672579,
      1, 0.615695751684985, 0.585237668902095, 1, 0.63700885566586,
      0.574632808733525, 0.629375017068631, 0.554673474405019, 0, 0.663439563471069,
      0.551905774349665, 0, 0.999980042563833, 0.733408085625146, 0.588094813990417,
      0.733408085625146, 0.588325146972442, 0, 0.999903422499639, 0.596195278327381,
      0.588325146972442, 0.589020320549427, 0.587862502354448, 0, 0.733408085625146,
      0.587645379940411, 0.581568453647645, 0.999939645751583, 0.681164869034425,
      0.596195278327381, 0.999972186404575, 0.589020320549427, 0.588272242526939,
      0.999808841326863, 0.591249554181622, 0.588137890314425, 0.590124953892591,
      0.587702786316262, 0.00168924881202357, 0.601310420216751, 0.587585754601369,
      0, 1, 0.684395501621856, 0.588832333654823, 0.684395501621856,
      0.612521029360702, 0, 1, 0.658247723637025, 0.612521029360702,
      0.635690181201618, 0.564725480535334, 0, 0.684395501621856, 0.545212037997334,
      0.505827784911249, 1, 0.680112159860339, 0.658247723637025, 1,
      0.635690181201618, 0.607821734467531, 1, 0.649303564315729, 0.593606892099468,
      0.644791403066015, 0.550105778650825, 1.95688859353457e-06, 0.662678470095522,
      0.540309586231236, 0, 1, 0.667709024688218, 0.611808833844179,
      0.667709024688218, 0.637639124326372, 0.138765913412546, 1, 0.661667769971892,
      0.637639124326372, 0.652758332078045, 0.567424290186004, 0.00131929497785432,
      0.667709024688218, 0.510079434870597, 0.305603208872524, 1, 0.666945137803983,
      0.661667769971892, 1, 0.652758332078045, 0.633521553424334, 1,
      0.658653072080534, 0.618145332848138, 0.656891089566529, 0.526753468207694,
      0.170227131495712, 0.662954212190577, 0.491611369996798, 0.000133267456484988,
      1, 0.733772582465577, 0.570224300934701, 0.733772582465577, 0.592156545325146,
      0, 1, 0.648505042223343, 0.592156545325146, 0.617688194266119,
      0.551496346461226, 0, 0.733772582465577, 0.538578811374203, 0.517272260831303,
      1, 0.707462050155765, 0.648505042223343, 1, 0.617688194266119,
      0.587496997871983, 0.999997812767346, 0.635195268121854, 0.57434280126587,
      0.629117104586359, 0.541647799775378, 0, 0.656114454122126, 0.535611906621616,
      0
    )
  )

  # gaussian
  predictions <- plyr::llply(1:10, function(s) {
    xpectr::set_test_seed(s)
    nn <- nnet::nnet(as.formula("score~diagnosis"), data = dat, size = 10, linout = TRUE)
    as.vector(predict(nn, dat))
  }) %>% unlist()

  expect_equal(
    predictions,
    c(
      30.6666665924374, 30.6666665924374, 30.6666665924374, 50.9166668310158,
      50.9166668310158, 50.9166668310158, 30.6666665924374, 30.6666665924374,
      30.6666665924374, 50.9166668310158, 50.9166668310158, 50.9166668310158,
      30.6666665924374, 30.6666665924374, 30.6666665924374, 30.6666665924374,
      30.6666665924374, 30.6666665924374, 30.6666665924374, 30.6666665924374,
      30.6666665924374, 30.6666665924374, 30.6666665924374, 30.6666665924374,
      50.9166668310158, 50.9166668310158, 50.9166668310158, 50.9166668310158,
      50.9166668310158, 50.9166668310158, 30.6665784709863, 30.6665784709863,
      30.6665784709863, 50.9166708797146, 50.9166708797146, 50.9166708797146,
      30.6665784709863, 30.6665784709863, 30.6665784709863, 50.9166708797146,
      50.9166708797146, 50.9166708797146, 30.6665784709863, 30.6665784709863,
      30.6665784709863, 30.6665784709863, 30.6665784709863, 30.6665784709863,
      30.6665784709863, 30.6665784709863, 30.6665784709863, 30.6665784709863,
      30.6665784709863, 30.6665784709863, 50.9166708797146, 50.9166708797146,
      50.9166708797146, 50.9166708797146, 50.9166708797146, 50.9166708797146,
      30.6666669812194, 30.6666669812194, 30.6666669812194, 50.9166667804219,
      50.9166667804219, 50.9166667804219, 30.6666669812194, 30.6666669812194,
      30.6666669812194, 50.9166667804219, 50.9166667804219, 50.9166667804219,
      30.6666669812194, 30.6666669812194, 30.6666669812194, 30.6666669812194,
      30.6666669812194, 30.6666669812194, 30.6666669812194, 30.6666669812194,
      30.6666669812194, 30.6666669812194, 30.6666669812194, 30.6666669812194,
      50.9166667804219, 50.9166667804219, 50.9166667804219, 50.9166667804219,
      50.9166667804219, 50.9166667804219, 38.7667925707427, 38.7667925707427,
      38.7667925707427, 38.7667588942958, 38.7667588942958, 38.7667588942958,
      38.7667925707427, 38.7667925707427, 38.7667925707427, 38.7667588942958,
      38.7667588942958, 38.7667588942958, 38.7667925707427, 38.7667925707427,
      38.7667925707427, 38.7667925707427, 38.7667925707427, 38.7667925707427,
      38.7667925707427, 38.7667925707427, 38.7667925707427, 38.7667925707427,
      38.7667925707427, 38.7667925707427, 38.7667588942958, 38.7667588942958,
      38.7667588942958, 38.7667588942958, 38.7667588942958, 38.7667588942958,
      30.6666660461772, 30.6666660461772, 30.6666660461772, 50.9166644466571,
      50.9166644466571, 50.9166644466571, 30.6666660461772, 30.6666660461772,
      30.6666660461772, 50.9166644466571, 50.9166644466571, 50.9166644466571,
      30.6666660461772, 30.6666660461772, 30.6666660461772, 30.6666660461772,
      30.6666660461772, 30.6666660461772, 30.6666660461772, 30.6666660461772,
      30.6666660461772, 30.6666660461772, 30.6666660461772, 30.6666660461772,
      50.9166644466571, 50.9166644466571, 50.9166644466571, 50.9166644466571,
      50.9166644466571, 50.9166644466571, 30.666546478818, 30.666546478818,
      30.666546478818, 50.9166831547643, 50.9166831547643, 50.9166831547643,
      30.666546478818, 30.666546478818, 30.666546478818, 50.9166831547643,
      50.9166831547643, 50.9166831547643, 30.666546478818, 30.666546478818,
      30.666546478818, 30.666546478818, 30.666546478818, 30.666546478818,
      30.666546478818, 30.666546478818, 30.666546478818, 30.666546478818,
      30.666546478818, 30.666546478818, 50.9166831547643, 50.9166831547643,
      50.9166831547643, 50.9166831547643, 50.9166831547643, 50.9166831547643,
      30.6666863919472, 30.6666863919472, 30.6666863919472, 50.9167499804574,
      50.9167499804574, 50.9167499804574, 30.6666863919472, 30.6666863919472,
      30.6666863919472, 50.9167499804574, 50.9167499804574, 50.9167499804574,
      30.6666863919472, 30.6666863919472, 30.6666863919472, 30.6666863919472,
      30.6666863919472, 30.6666863919472, 30.6666863919472, 30.6666863919472,
      30.6666863919472, 30.6666863919472, 30.6666863919472, 30.6666863919472,
      50.9167499804574, 50.9167499804574, 50.9167499804574, 50.9167499804574,
      50.9167499804574, 50.9167499804574, 30.6666666743281, 30.6666666743281,
      30.6666666743281, 50.9166673429457, 50.9166673429457, 50.9166673429457,
      30.6666666743281, 30.6666666743281, 30.6666666743281, 50.9166673429457,
      50.9166673429457, 50.9166673429457, 30.6666666743281, 30.6666666743281,
      30.6666666743281, 30.6666666743281, 30.6666666743281, 30.6666666743281,
      30.6666666743281, 30.6666666743281, 30.6666666743281, 30.6666666743281,
      30.6666666743281, 30.6666666743281, 50.9166673429457, 50.9166673429457,
      50.9166673429457, 50.9166673429457, 50.9166673429457, 50.9166673429457,
      30.6669757332435, 30.6669757332435, 30.6669757332435, 50.9159142236694,
      50.9159142236694, 50.9159142236694, 30.6669757332435, 30.6669757332435,
      30.6669757332435, 50.9159142236694, 50.9159142236694, 50.9159142236694,
      30.6669757332435, 30.6669757332435, 30.6669757332435, 30.6669757332435,
      30.6669757332435, 30.6669757332435, 30.6669757332435, 30.6669757332435,
      30.6669757332435, 30.6669757332435, 30.6669757332435, 30.6669757332435,
      50.9159142236694, 50.9159142236694, 50.9159142236694, 50.9159142236694,
      50.9159142236694, 50.9159142236694, 30.6666580877896, 30.6666580877896,
      30.6666580877896, 50.9166717465376, 50.9166717465376, 50.9166717465376,
      30.6666580877896, 30.6666580877896, 30.6666580877896, 50.9166717465376,
      50.9166717465376, 50.9166717465376, 30.6666580877896, 30.6666580877896,
      30.6666580877896, 30.6666580877896, 30.6666580877896, 30.6666580877896,
      30.6666580877896, 30.6666580877896, 30.6666580877896, 30.6666580877896,
      30.6666580877896, 30.6666580877896, 50.9166717465376, 50.9166717465376,
      50.9166717465376, 50.9166717465376, 50.9166717465376, 50.9166717465376
    )
  )

  # multinom

  dat2 <- dat %>%
    rbind(
      dat %>%
        dplyr::mutate(diagnosis = as.factor(as.numeric(as.character(diagnosis)) + 3))
    )

  predictions <- plyr::llply(1:10, function(s) {
    xpectr::set_test_seed(s)
    nn <- nnet::multinom(as.formula("diagnosis~score"), data = dat2)
    as.vector(predict(nn, dat2))
  }) %>% unlist()

  expect_equal(
    predictions,
    c(
      "1", "1", "4", "1", "4", "0", "1", "1", "4", "1", "3", "0",
      "1", "0", "0", "1", "1", "1", "1", "1", "4", "1", "1", "4", "1",
      "0", "0", "1", "0", "0", "1", "1", "4", "1", "4", "0", "1", "1",
      "4", "1", "3", "0", "1", "0", "0", "1", "1", "1", "1", "1", "4",
      "1", "1", "4", "1", "0", "0", "1", "0", "0", "1", "1", "4", "1",
      "4", "0", "1", "1", "4", "1", "3", "0", "1", "0", "0", "1", "1",
      "1", "1", "1", "4", "1", "1", "4", "1", "0", "0", "1", "0", "0",
      "1", "1", "4", "1", "4", "0", "1", "1", "4", "1", "0", "0", "1",
      "0", "0", "1", "1", "1", "1", "1", "4", "1", "1", "4", "1", "0",
      "0", "1", "0", "0", "1", "1", "4", "1", "4", "0", "1", "1", "4",
      "1", "3", "0", "1", "0", "0", "1", "1", "1", "1", "1", "4", "1",
      "1", "4", "1", "0", "0", "1", "0", "0", "1", "1", "4", "1", "4",
      "0", "1", "1", "4", "1", "0", "0", "1", "0", "0", "1", "1", "1",
      "1", "1", "4", "1", "1", "4", "1", "0", "0", "1", "0", "0", "1",
      "1", "4", "1", "4", "0", "1", "1", "4", "1", "0", "0", "1", "0",
      "0", "1", "1", "1", "1", "1", "4", "1", "1", "4", "1", "0", "0",
      "1", "0", "0", "1", "1", "4", "1", "4", "0", "1", "1", "4", "1",
      "3", "0", "1", "0", "0", "1", "1", "1", "1", "1", "4", "1", "1",
      "4", "1", "0", "0", "1", "0", "0", "1", "1", "4", "1", "4", "0",
      "1", "1", "4", "1", "3", "0", "1", "0", "0", "1", "1", "1", "1",
      "1", "4", "1", "1", "4", "1", "0", "0", "1", "0", "0", "1", "1",
      "4", "1", "4", "0", "1", "1", "4", "1", "0", "0", "1", "0", "0",
      "1", "1", "1", "1", "1", "4", "1", "1", "4", "1", "0", "0", "1",
      "0", "0", "1", "1", "4", "1", "4", "0", "1", "1", "4", "1", "0",
      "0", "1", "0", "0", "1", "1", "1", "1", "1", "4", "1", "1", "4",
      "1", "0", "0", "1", "0", "0", "1", "1", "4", "1", "4", "0", "1",
      "1", "4", "1", "0", "0", "1", "0", "0", "1", "1", "1", "1", "1",
      "4", "1", "1", "4", "1", "0", "0", "1", "0", "0", "1", "1", "4",
      "1", "4", "0", "1", "1", "4", "1", "0", "0", "1", "0", "0", "1",
      "1", "1", "1", "1", "4", "1", "1", "4", "1", "0", "0", "1", "0",
      "0", "1", "1", "4", "1", "4", "0", "1", "1", "4", "1", "3", "0",
      "1", "0", "0", "1", "1", "1", "1", "1", "4", "1", "1", "4", "1",
      "0", "0", "1", "0", "0", "1", "1", "4", "1", "4", "0", "1", "1",
      "4", "1", "3", "0", "1", "0", "0", "1", "1", "1", "1", "1", "4",
      "1", "1", "4", "1", "0", "0", "1", "0", "0", "1", "1", "4", "1",
      "4", "0", "1", "1", "4", "1", "3", "0", "1", "0", "0", "1", "1",
      "1", "1", "1", "4", "1", "1", "4", "1", "0", "0", "1", "0", "0",
      "1", "1", "4", "1", "4", "0", "1", "1", "4", "1", "3", "0", "1",
      "0", "0", "1", "1", "1", "1", "1", "4", "1", "1", "4", "1", "0",
      "0", "1", "0", "0", "1", "1", "4", "1", "4", "0", "1", "1", "4",
      "1", "3", "0", "1", "0", "0", "1", "1", "1", "1", "1", "4", "1",
      "1", "4", "1", "0", "0", "1", "0", "0", "1", "1", "4", "1", "4",
      "0", "1", "1", "4", "1", "0", "0", "1", "0", "0", "1", "1", "1",
      "1", "1", "4", "1", "1", "4", "1", "0", "0", "1", "0", "0", "1",
      "1", "4", "1", "4", "0", "1", "1", "4", "1", "3", "0", "1", "0",
      "0", "1", "1", "1", "1", "1", "4", "1", "1", "4", "1", "0", "0",
      "1", "0", "0"
    )
  )
})

test_that("glmer throws same warnings on mac and ubuntu", {
  testthat::skip("mac and ubuntu give different warnings")
  # Tested on both platforms on travis as well
  # 13 warnings on mac, 16 on ubuntu
  # Local test should run on mac as is

  xpectr::set_test_seed(10)

  dat <- participant.scores %>%
    dplyr::mutate(diagnosis = as.factor(diagnosis))

  formula <- "diagnosis ~ score + age + (1|session) + (1|age)"
  family <- "binomial"
  REML <- FALSE
  control <- lme4::glmerControl(optimizer = "bobyqa")

  warnings_and_messages <- plyr::llply(1:10, function(s) {
    process_ <- testthat::evaluate_promise({
      xpectr::set_test_seed(s)
      lme4::glmer(
        formula = formula, family = family, data = dplyr::sample_frac(dat, 0.95),
        REML = REML, control = control
      )
    })
    list(
      "warnings" = process_$warnings,
      "messages" = process_$messages
    )
  })
  warns <- unlist(warnings_and_messages %c% "warnings")
  mesgs <- unlist(warnings_and_messages %c% "messages")

  # NOTE devtools::test() used '' but console outputted ‘’
  # which is weird.. but now I'm just stripping the punctuations
  # cause it's not a problem here
  expect_equal(
    xpectr::strip(sort(warns), remove_numbers = TRUE),
    xpectr::strip(sort(
      c(
        "extra argument(s) ‘REML’ disregarded", "extra argument(s) ‘REML’ disregarded",
        "extra argument(s) ‘REML’ disregarded", "extra argument(s) ‘REML’ disregarded",
        "extra argument(s) ‘REML’ disregarded", "extra argument(s) ‘REML’ disregarded",
        "extra argument(s) ‘REML’ disregarded", "extra argument(s) ‘REML’ disregarded",
        "extra argument(s) ‘REML’ disregarded", "extra argument(s) ‘REML’ disregarded",
        "Model failed to converge with max|grad| = 0.0161905 (tol = 0.001, component 1)",
        "Model failed to converge with max|grad| = 0.025616 (tol = 0.001, component 1)",
        "Model failed to converge with max|grad| = 0.033809 (tol = 0.001, component 1)"
      )
    ), remove_numbers = TRUE)
  )

  expect_equal(mesgs, character())
})

test_that("one-hot encoding helper works for evaluate purposes", {
  df <- tibble::tibble(
    "Target" = c("D", rep(c("A", "B", "C"), 3)),
    "PredictedClass" = c("A", rep(c("A", "B", "C"), 3))
  )

  df_large <- dplyr::bind_rows(rep(list(df), 100)) %>%
    dplyr::mutate(x = runif(nrow(df) * 100))

  # system.time(one_hot_encode(df_large, "Target", c_levels=c("D","A","C","B","F")))
  oh1 <- one_hot_encode(df, "Target")
  oh2 <- one_hot_encode(df, "Target", c_levels = c("D", "A", "C", "B", "F"))

  expect_equal(
    oh1,
    structure(
      list(
        Target = c(
          "D", "A", "B", "C", "A", "B", "C",
          "A", "B", "C"
        ),
        PredictedClass = c(
          "A", "A", "B", "C", "A", "B",
          "C", "A", "B", "C"
        ),
        A = c(0L, 1L, 0L, 0L, 1L, 0L, 0L, 1L, 0L, 0L),
        B = c(0L, 0L, 1L, 0L, 0L, 1L, 0L, 0L, 1L, 0L),
        C = c(0L, 0L, 0L, 1L, 0L, 0L, 1L, 0L, 0L, 1L),
        D = c(1L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L)
      ),
      row.names = c(NA, -10L),
      class = c("tbl_df", "tbl", "data.frame")
    )
  )

  expect_equal(
    oh2,
    structure(
      list(
        Target = c(
          "D", "A", "B", "C", "A", "B", "C",
          "A", "B", "C"
        ),
        PredictedClass = c(
          "A", "A", "B", "C", "A", "B",
          "C", "A", "B", "C"
        ),
        A = c(0L, 1L, 0L, 0L, 1L, 0L, 0L, 1L, 0L, 0L),
        B = c(0L, 0L, 1L, 0L, 0L, 1L, 0L, 0L, 1L, 0L),
        C = c(0L, 0L, 0L, 1L, 0L, 0L, 1L, 0L, 0L, 1L),
        D = c(1L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L),
        F = c(0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L)
      ),
      row.names = c(NA, -10L),
      class = c("tbl_df", "tbl", "data.frame")
    )
  )
})

test_that("non_empty_names()", {

  a <- list(NULL, "a" = 3, f = 8)
  names(a) <- c(NA, "a", NULL)

  # xpectr::gxs_function(non_empty_names,
  #                      args_values = list(
  #                        "x" = list(
  #                          list("a" = 1, "b" = 3, 4, NA),
  #                          list(1,2,3,4),
  #                          a,
  #                          # NA,
  #                          c("a" = 1, 3, "p" = NA),
  #                          list("a" = list("b" = 3))
  #                        )
  #                      ))

  ## Testing 'non_empty_names'                                                ####
  ## Initially generated by xpectr
  # Testing different combinations of argument values

  # Testing non_empty_names(x = list(1, 2, 3, 4))
  # Assigning output
  output_12935 <- non_empty_names(x = list(1, 2, 3, 4))
  # Testing class
  expect_equal(
    class(output_12935),
    "character",
    fixed = TRUE)
  # Testing type
  expect_type(
    output_12935,
    type = "character")
  # Testing values
  expect_equal(
    output_12935,
    character(0),
    fixed = TRUE)
  # Testing names
  expect_equal(
    names(output_12935),
    NULL,
    fixed = TRUE)
  # Testing length
  expect_equal(
    length(output_12935),
    0L)
  # Testing sum of element lengths
  expect_equal(
    sum(xpectr::element_lengths(output_12935)),
    0L)

  # Testing non_empty_names(x = a)
  # Testing class
  expect_equal(
    class(non_empty_names(x = a)),
    "character",
    fixed = TRUE)
  # Testing type
  expect_type(
    non_empty_names(x = a),
    type = "character")
  # Testing values
  expect_equal(
    non_empty_names(x = a),
    "a",
    fixed = TRUE)
  # Testing names
  expect_equal(
    names(non_empty_names(x = a)),
    NULL,
    fixed = TRUE)
  # Testing length
  expect_equal(
    length(non_empty_names(x = a)),
    1L)
  # Testing sum of element lengths
  expect_equal(
    sum(xpectr::element_lengths(non_empty_names(x = a))),
    1L)

  # Testing non_empty_names(x = c(a = 1, 3, p = NA))
  # Assigning output
  output_14590 <- non_empty_names(x = c(a = 1, 3, p = NA))
  # Testing class
  expect_equal(
    class(output_14590),
    "character",
    fixed = TRUE)
  # Testing type
  expect_type(
    output_14590,
    type = "character")
  # Testing values
  expect_equal(
    output_14590,
    c("a", "p"),
    fixed = TRUE)
  # Testing names
  expect_equal(
    names(output_14590),
    NULL,
    fixed = TRUE)
  # Testing length
  expect_equal(
    length(output_14590),
    2L)
  # Testing sum of element lengths
  expect_equal(
    sum(xpectr::element_lengths(output_14590)),
    2L)

  # Testing non_empty_names(x = list(a = list(b = 3)))
  # Assigning output
  output_13323 <- non_empty_names(x = list(a = list(b = 3)))
  # Testing class
  expect_equal(
    class(output_13323),
    "character",
    fixed = TRUE)
  # Testing type
  expect_type(
    output_13323,
    type = "character")
  # Testing values
  expect_equal(
    output_13323,
    "a",
    fixed = TRUE)
  # Testing names
  expect_equal(
    names(output_13323),
    NULL,
    fixed = TRUE)
  # Testing length
  expect_equal(
    length(output_13323),
    1L)
  # Testing sum of element lengths
  expect_equal(
    sum(xpectr::element_lengths(output_13323)),
    1L)

  # Testing non_empty_names(x = list(a = 1, b = 3, 4, NA))
  # Assigning output
  output_16508 <- non_empty_names(x = list(a = 1, b = 3, 4, NA))
  # Testing class
  expect_equal(
    class(output_16508),
    "character",
    fixed = TRUE)
  # Testing type
  expect_type(
    output_16508,
    type = "character")
  # Testing values
  expect_equal(
    output_16508,
    c("a", "b"),
    fixed = TRUE)
  # Testing names
  expect_equal(
    names(output_16508),
    NULL,
    fixed = TRUE)
  # Testing length
  expect_equal(
    length(output_16508),
    2L)
  # Testing sum of element lengths
  expect_equal(
    sum(xpectr::element_lengths(output_16508)),
    2L)

  # Testing non_empty_names(x = NULL)
  # Assigning output
  output_12579 <- non_empty_names(x = NULL)
  # Testing class
  expect_equal(
    class(output_12579),
    "character",
    fixed = TRUE)
  # Testing type
  expect_type(
    output_12579,
    type = "character")
  # Testing values
  expect_equal(
    output_12579,
    character(0),
    fixed = TRUE)
  # Testing names
  expect_equal(
    names(output_12579),
    NULL,
    fixed = TRUE)
  # Testing length
  expect_equal(
    length(output_12579),
    0L)
  # Testing sum of element lengths
  expect_equal(
    sum(xpectr::element_lengths(output_12579)),
    0L)

  ## Finished testing 'non_empty_names'                                       ####

})

test_that("reposition_column()", {

  data <- participant.scores


  ## Testing 'data'                                                         ####
  ## Initially generated by xpectr
  xpectr::set_test_seed(42)
  # Testing class
  expect_equal(
    class(data),
    "data.frame",
    fixed = TRUE)
  # Testing column values
  expect_equal(
    data[["participant"]],
    structure(c(1L, 1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L, 4L, 4L, 4L, 5L,
      5L, 5L, 6L, 6L, 6L, 7L, 7L, 7L, 8L, 8L, 8L, 9L, 9L, 9L, 10L,
      10L, 10L), .Label = c("1", "2", "3", "4", "5", "6", "7", "8",
      "9", "10"), class = "factor"))
  expect_equal(
    data[["age"]],
    c(20, 20, 20, 23, 23, 23, 27, 27, 27, 21, 21, 21, 32, 32, 32, 31,
      31, 31, 43, 43, 43, 21, 21, 21, 34, 34, 34, 32, 32, 32),
    tolerance = 1e-4)
  expect_equal(
    data[["diagnosis"]],
    c(1, 1, 1, 0, 0, 0, 1, 1, 1, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 0, 0, 0, 0, 0, 0),
    tolerance = 1e-4)
  expect_equal(
    data[["score"]],
    c(10, 24, 45, 24, 40, 67, 15, 30, 40, 35, 50, 78, 24, 54, 62, 14,
      25, 30, 11, 35, 41, 16, 32, 44, 33, 53, 66, 29, 55, 81),
    tolerance = 1e-4)
  expect_equal(
    data[["session"]],
    c(1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3,
      1, 2, 3, 1, 2, 3, 1, 2, 3),
    tolerance = 1e-4)
  # Testing column names
  expect_equal(
    names(data),
    c("participant", "age", "diagnosis", "score", "session"),
    fixed = TRUE)
  # Testing column classes
  expect_equal(
    xpectr::element_classes(data),
    c("factor", "numeric", "numeric", "numeric", "integer"),
    fixed = TRUE)
  # Testing column types
  expect_equal(
    xpectr::element_types(data),
    c("integer", "double", "double", "double", "integer"),
    fixed = TRUE)
  # Testing dimensions
  expect_equal(
    dim(data),
    c(30L, 5L))
  # Testing group keys
  expect_equal(
    colnames(dplyr::group_keys(data)),
    character(0),
    fixed = TRUE)
  ## Finished testing 'data'                                                ####

  data <- reposition_column(data, "age", .before = "session")

  ## Testing 'data'                                                         ####
  ## Initially generated by xpectr
  xpectr::set_test_seed(42)
  # Testing class
  expect_equal(
    class(data),
    "data.frame",
    fixed = TRUE)
  # Testing column values
  expect_equal(
    data[["participant"]],
    structure(c(1L, 1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L, 4L, 4L, 4L, 5L,
      5L, 5L, 6L, 6L, 6L, 7L, 7L, 7L, 8L, 8L, 8L, 9L, 9L, 9L, 10L,
      10L, 10L), .Label = c("1", "2", "3", "4", "5", "6", "7", "8",
      "9", "10"), class = "factor"))
  expect_equal(
    data[["diagnosis"]],
    c(1, 1, 1, 0, 0, 0, 1, 1, 1, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 0, 0, 0, 0, 0, 0),
    tolerance = 1e-4)
  expect_equal(
    data[["score"]],
    c(10, 24, 45, 24, 40, 67, 15, 30, 40, 35, 50, 78, 24, 54, 62, 14,
      25, 30, 11, 35, 41, 16, 32, 44, 33, 53, 66, 29, 55, 81),
    tolerance = 1e-4)
  expect_equal(
    data[["age"]],
    c(20, 20, 20, 23, 23, 23, 27, 27, 27, 21, 21, 21, 32, 32, 32, 31,
      31, 31, 43, 43, 43, 21, 21, 21, 34, 34, 34, 32, 32, 32),
    tolerance = 1e-4)
  expect_equal(
    data[["session"]],
    c(1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3,
      1, 2, 3, 1, 2, 3, 1, 2, 3),
    tolerance = 1e-4)
  # Testing column names
  expect_equal(
    names(data),
    c("participant", "diagnosis", "score", "age", "session"),
    fixed = TRUE)
  # Testing column classes
  expect_equal(
    xpectr::element_classes(data),
    c("factor", "numeric", "numeric", "numeric", "integer"),
    fixed = TRUE)
  # Testing column types
  expect_equal(
    xpectr::element_types(data),
    c("integer", "double", "double", "double", "integer"),
    fixed = TRUE)
  # Testing dimensions
  expect_equal(
    dim(data),
    c(30L, 5L))
  # Testing group keys
  expect_equal(
    colnames(dplyr::group_keys(data)),
    character(0),
    fixed = TRUE)
  ## Finished testing 'data'                                                ####

  data <- reposition_column(data, "age", .after = "session")


  ## Testing 'data'                                                         ####
  ## Initially generated by xpectr
  xpectr::set_test_seed(42)
  # Testing class
  expect_equal(
    class(data),
    "data.frame",
    fixed = TRUE)
  # Testing column values
  expect_equal(
    data[["participant"]],
    structure(c(1L, 1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L, 4L, 4L, 4L, 5L,
      5L, 5L, 6L, 6L, 6L, 7L, 7L, 7L, 8L, 8L, 8L, 9L, 9L, 9L, 10L,
      10L, 10L), .Label = c("1", "2", "3", "4", "5", "6", "7", "8",
      "9", "10"), class = "factor"))
  expect_equal(
    data[["diagnosis"]],
    c(1, 1, 1, 0, 0, 0, 1, 1, 1, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 0, 0, 0, 0, 0, 0),
    tolerance = 1e-4)
  expect_equal(
    data[["score"]],
    c(10, 24, 45, 24, 40, 67, 15, 30, 40, 35, 50, 78, 24, 54, 62, 14,
      25, 30, 11, 35, 41, 16, 32, 44, 33, 53, 66, 29, 55, 81),
    tolerance = 1e-4)
  expect_equal(
    data[["session"]],
    c(1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3,
      1, 2, 3, 1, 2, 3, 1, 2, 3),
    tolerance = 1e-4)
  expect_equal(
    data[["age"]],
    c(20, 20, 20, 23, 23, 23, 27, 27, 27, 21, 21, 21, 32, 32, 32, 31,
      31, 31, 43, 43, 43, 21, 21, 21, 34, 34, 34, 32, 32, 32),
    tolerance = 1e-4)
  # Testing column names
  expect_equal(
    names(data),
    c("participant", "diagnosis", "score", "session", "age"),
    fixed = TRUE)
  # Testing column classes
  expect_equal(
    xpectr::element_classes(data),
    c("factor", "numeric", "numeric", "integer", "numeric"),
    fixed = TRUE)
  # Testing column types
  expect_equal(
    xpectr::element_types(data),
    c("integer", "double", "double", "integer", "double"),
    fixed = TRUE)
  # Testing dimensions
  expect_equal(
    dim(data),
    c(30L, 5L))
  # Testing group keys
  expect_equal(
    colnames(dplyr::group_keys(data)),
    character(0),
    fixed = TRUE)
  ## Finished testing 'data'                                                ####


})

