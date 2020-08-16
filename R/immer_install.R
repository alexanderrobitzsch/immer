## File Name: immer_install.R
## File Version: 0.14
## File Name: immer_install.R
## File Version: 0.12

#####################################################
# installation of the free FACETS DOS_Version
# for this the DOSBOX is needed
immer_install <- function(DosBox_path = NULL, Facets_path = NULL){
  os_system <- Sys.info()["sysname"]
  user <- Sys.info()['user']
  # Ausgabe fuer den User
  switch(os_system,
         Windows=
{cat("I'm a Windows PC. Install Facets and Dosbox for Windows \n")},
Linux=
{cat("I'm a penguin. Install Facets and Dosbox for Linux \n")},
Darwin=
{cat("I'm a Mac. Install Facets and Dosbox for Mac \n")}
  )

if(os_system == "Windows"){
  # Link for dos-version of FACETS
  link_Facets <- "http://www.winsteps.com/a/facdos.zip"
  
  # Facets_path <-
  #   file.path("C:","Users",user,"Downloads")
  # win DOSbox
  # JS Verison 0.09: switched to portable Version of DosBox (no admin privileges needed)
  link_DosBox <-
    "https://download3.portableapps.com/portableapps/DOSBoxPortable/DOSBoxPortable_0.74.3.paf.exe?20190321"
  # where
  if(is.null(DosBox_path)){
    destination_dosBox <-
      paste0("C:\\Users\\",user,"\\Downloads\\")
  } else {
    destination_dosBox <- 
      DosBox_path
  }
  if (!dir.exists(file.path(destination_dosBox))) { 
      dir.create(file.path(destination_dosBox))
    }

  if(is.null(Facets_path)){
    destination_facets <-
      paste0("C:\\Users\\",user,"\\Documents\\facets")
  } else {
    destination_facets <- 
      Facets_path
  }

  if (!dir.exists(file.path(destination_facets))) { 
    dir.create(file.path(destination_facets))
  }
  
  # -----------------------------------------
  # download files
  error_facets <- tryCatch(
    download.file(
      url = link_Facets,
      destfile = file.path(destination_facets,"facdos.zip"),
      method = "internal"
    )
  )
  # JS Verison 0.09: changed method to 'auto'
  error_DosBox <- tryCatch(
    download.file(
      url = link_DosBox,
      destfile = file.path(destination_dosBox,"DOSBoxPortable.exe"),
      method = "auto",
      mode = "wb"
    )
  )
  cat( "install the DOSbox: \n")
  error_DosBox_install <- tryCatch(
    shell(file.path(destination_dosBox,"DOSBoxPortable.exe"))
  )
  # -----------------------------------------

  # Den Admin des Computers herausfinden: und Installation von DosBox
  # -----------------------------------------
    if(error_DosBox != 0){
      cat("Attention, there was an error while downloading the DosBox,
          please try again or install die DosBox manually \n")
      cat(paste0("for the manual installation pleas go to: \n",link_DosBox,"\n",
                 "after the download process finished we recomand to install
                 the DosBox into \n--> \\Users\\yourUser\\Documents <--"))
    }

    if(error_DosBox == 0){
    # JS Verison 0.09: changed installation process, no admin privilegs needed

      writeLines(c("...starting installation process of DosBox",
      "Please note the installation path, which will be needed later. e.g.:",
      paste0("--> C:\\Users\\",user,"\\Documents <--\n")))
      # admin <- system("net localgroup",intern=TRUE)
      # findeAdmin <- paste0("net localgroup ",gsub("\\*","",admin[grep("adm|Adm",admin)]))
      # test <- system(findeAdmin,intern=TRUE)
      # administrator <- test[grep("--",test)+1]
      system("cmd.exe", input = paste0("start ",destination_dosBox))

      # -----------------------------------------
      # Edit the configFile to speed up the process [if the installation is successful]
      # if(!is.na(match("DOSBox",list.files(paste0("C:\\Users\\",user,"\\AppData\\Local\\"))))){
      #   config <- readLines(paste0("C:\\Users\\",user,"\\AppData\\Local\\DOSBox\\dosbox-0.74.conf"))
      #   config[grep("cycles=",config)] <- "cycles=max"
      #   writeLines(config,paste0("C:\\Users\\",user,"\\AppData\\Local\\DOSBox\\dosbox-0.74.conf"))
      #   }
    }
    if(error_facets!=0){
      cat("Attention, there was an error while downloading facets,
          please try again or install die DosBox manually \n")
      cat(paste0("for the manual installation pleas go to: \n",link_Facets,"\n",
                 "after the download process finished we recomand to unzip the Folder and
                 move the content to \n-->",destination_facets,"<--"))
    }
    if(error_facets==0){
      cat("unzip facets\n")
      utils::unzip (file.path(destination_facets,"facdos.zip"), exdir=file.path(destination_facets,"facdos"))
      cat("move facets to ",destination_facets,"\n")
      # cat("remove facets.zip \n")
      # system(paste("del",file.path(destination_facets,"facdos.zip")))
      # -----------------------------------------
    }
  } # End Windows

  # LINUX
  if(os_system=="Linux"){
    # Link fuer Facets
    link_Facets <- "http://www.winsteps.com/a/facdos.zip"
    # wo hinein
    destination_facets <- "/home/Downloads/facdos.zip"
    # win DOSbox
    link_DosBox <- "http://sourceforge.net/projects/dosbox/files/dosbox/0.74/dosbox-0.74.tar.gz/download"
    # wohinein
    destination_dosBox <- "/home/Downloads/Downloads/dosbox-0.74.tar.gz"

    cat("\n install automake for dosbox \n")
    system("sudo -kS apt-get -y install build-essential autoconf automake",input=readline("Enter your password: "))

    cat("\n install dosbox \n")
    system("sudo -kS apt-get -y install build-dep dosbox",input=readline("Enter your password: "))
    cat("\n DONE \n")

    cat("\n unzip facdos to ",paste0("/home/",user,"/facdos \n"))
    system(paste0("unzip -o ", destination_facets," -d /home/",user,"/facdos"))

    cat("\n now you will find dosbox and facdos in:",paste0("/home/",user,"/facdos"))
  } # End Linux

  # MAC-OS X
  if(os_system=="Darwin"){
    # Link fuer Facets
    link_Facets <- "http://www.winsteps.com/a/facdos.zip"
    # wo hinein
    destination_facets <- "~/Downloads/facdos.zip"
    # win DOSbox
    link_DosBox <- "https://sourceforge.net/projects/dosbox/files/dosbox/0.74-3/DOSBox-0.74-3.dmg/download"
    # wohinein
    destination_dosBox <- "~/Downloads/DOSBox-0.74-1_Universal.dmg"

    # -----------------------------------------
    # Files herunterladen
    error_facets <- tryCatch(
      download.file(
        url=link_Facets,
        destfile=destination_facets,
        method="internal"
      )
    )
    error_DosBox <- tryCatch(
      download.file(
        url=link_DosBox,
        destfile=destination_dosBox
      )
    )
    if(error_facets==0){
      cat("unzip facets\n")
      utils::unzip(destination_facets, exdir="~/Downloads/facdos")
      cat("unzip facets to '~/Downloads/facdos' \n")
      # -----------------------------------------
    }
    # -----------------------------------------
  } # End Mac OS X

}
#############################################################################
