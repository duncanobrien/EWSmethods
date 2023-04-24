test_that("EWSNet can be correctly initialised", {

  skip_if_no_miniconda <- function(){

    miniconda_path <- if(identical(.Platform$OS.type, "windows")){
      "condabin/conda.bat"}else{
        "bin/conda"}

    miniconda <- file.exists(file.path(reticulate::miniconda_path(), miniconda_path))

    if(!miniconda)
      skip("miniconda not available for testing")
  }

  skip_if_no_miniconda()

  bypass_reticulate_autoinit()
  ewsnet_init(envname = "test_env", auto = TRUE)
  expect_match(reticulate::py_config()$python,"test_env")

  detect.sys <- paste(Sys.info()["sysname"],Sys.info()["machine"],sep = "_")
  if(detect.sys == "Darwin_arm64"){
    expect_true("tensorflow-macos" %in% reticulate::py_list_packages()$package)
  }else{
    expect_true("tensorflow" %in% reticulate::py_list_packages()$package)
  }
  #reticulate::miniconda_uninstall()
  #conda <- try(reticulate::conda_list())
  #expect_true(inherits(conda, "try-error"))
  #expect_true(!("test_env" %in% reticulate::conda_list()$name))

})
