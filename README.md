
# monitorMTF

<!-- badges: start -->
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The goal of `{monitorMTF}` is to provide a set of user-friendly tools for monitoring Multi-Tier Framework surveys worldwide.

In particular, `{monitorMTF}` aims to:

- Offer a user-friendly interface, through an interactive graphical application
- Execute several workflows for users with the click of a button:
  - Download all survey data
  - Validate all interviews--identifying issues (e.g., critical questions left unanswered, no food consumption reported, etc), recommending actions (e.g., reject), and enabling execution of recommended actions (e.g., click a button to reject interviews recommended for rejection with a comprehensive set of comments)
  - Generate data quality report
- Enable users to manage several survey projects at once, by creating a survey project, loading its settings, and executing workflows for that project

## Installation 🏗️

### Installing system dependencies 💻

For this application to work, the device running it must have:

- R (version 4.4.1 or more recent)
- RTools (version 4.4 or more recent)
- RStudio (version 2024.04.2+764 or more recent)
- Quarto (version 1.5.56 or more recent)

If these programs are installed, continue to the next heading.

Otherwise, ....

<details>
<summary>
... read detailed installation instructions here 👁️📑
</summary>

Before running this program for the first time, (re)install the
following software:

- [R](#r)
- [RTools](#rtools)
- [RStudio](#rstudio)
- [Quarto](#quarto)

Even if these software packages are already installed, it is necessary
to reinstall them in order to have the latest version of these tools for
this program to work successfully.

Please read below about how to install these programs.

#### R

- Follow this [link](https://cran.r-project.org/)
- Click on the appropriate link for your operating system
- Click on `base`
- Download and install (e.g.,
  [this](https://cran.r-project.org/bin/windows/base/R-4.3.2-win.exe)
  for Windows)

#### RTools

Required for the Windows operating system.

- Follow this [link](https://cran.r-project.org/)
- Click on `Windows`
- Click on `RTools`
- Download
  (e.g.,[this](https://cran.r-project.org/bin/windows/Rtools/rtools43/files/rtools43-5863-5818.exe)for
  a 64bit system)
- Install in the default installation location (e.g., `C:\rtools43` on
  Windows)

This program allows R to compile C++ scripts used by certain packages
(e.g., `{dplyr}`).

#### RStudio

- Follow this [link](https://posit.co/products/open-source/rstudio/)
- Click on the `DOWNLOAD RSTUDIO` button in the upper right-hand corner
  of the page’s navbar
- Click on the appropriate link for your operating system
- Download and install (e.g.,
  [this](https://download1.rstudio.org/electron/windows/RStudio-2023.09.1-494.exe)
  for Windows)

#### Quarto

- Follow this [link](https://quarto.org/docs/get-started/)
- Scroll to the `Step 1` header
- Download the appropriate installation file for your operating system--likely the one suggested in the `Download Quarto CLI` button

Note: while Quarto should be packaged with recent versions of RStudio, it is good to install Quarto separately, just in case.

</details>

### Install the package 📦

Since `{monitorMTF}` is not yet available on CRAN, it can be installed from
GitHub as follows:

``` r
if (!require("devtools")) install.packages("devtools")
devtools::install_github("arthur-shaw/monitorMTF")
```

## Usage 👩‍💻

### Launch 🚀

To launch the application,

- Open RStudio
- Execute the following commands in the console

```r
# load the package
library(monitorMTF)
# run the application
run_app()
```


For step-by-step usage instructions...

<details>
<summary>... please read here</summary>

- [Setup](#setup-️)
- [Download data](#download-data-️)
- [Validate / reject interviews](#validate--reject-interviews-)
- [Generating data quality report](#generate-data-quality-report-)

### Setup ⚙️

#### Choose a project

First, create a new project or a load an existing one.

To create a new project:

- Provide a short name for it
- Select it from the drop-down
- Press the `Load project` button

To load an existing project:

- Select it from the drop-down
- Press the `Load project` button

#### Provide project settings

To provide settings, click on the `Settings` entry in the top navigation bar. 

These settings include:

- **Server details.** These are needed for the application to interact with your server on your behalf (e.g., download data, reject interviews, etc).
- **Questionnaires.** The application needs to know which MTF questionnaires are part of your survey project, and which questionnaires that appear on the server are those whose data you need. For each questionnaire type, use the toggle to specify whether the questionnaire type is deployed or not. For questionnaires that are deployed, enter some text (or a regular expression) that identifies the questionnaire(s) of interest. Once the text has been composed, press the `Search` button to query the server. Once the desired questionnaire(s) is (are) shown, press the `Save` button. This may be an iterative process. These actions can be repeated as needed until the right questionnaires are identified.

### Download data ⬇️

There are two steps to downloading data:

1. [Fetch it from the server](#fetch-from-the-server)
2. [Get it outside of the application](#get-it-outside-the-application)

The second step is strictly optional. The application only requires the first step.

#### Fetch from the server

To download data from the server:

- Click on the `Get` entry in the application's top navigation bar.
- Press the `Fetch data` button.

#### Get it outside the application

To get these data for usage outside of the application: 

- Expand the `Download data` accordion on the right-hand side of the screen.
- Click the button of the data you would like to obtain (e.g., `Household` to get the combined data from the household questionnaires)

For convenience, the application downloads data from the server to a specific "hidden" folder. This ensures that the application finds the data where it expects and so that it can safely perform application-specific operations on it.

Mindful that end users may need downloaded data for other operations, the application allows the user to obtain the data that the application obtains or creates, and save it in an appropriate spot in the user's project file system.

### Validate / reject interviews ❌

The application executes the following workflow:

- **Validate.** The application includes a set of data validations to perform. To execute them, expand the `Validate` accordion and press the `Run` button.
- **Edit.** The application creates a file containing interviews recommended to reject. To see the contents of this file, simply expand the `Edit` accordion. To edit the contents of the file, edit as one would an Excel file. To either confirm the system-generated recommendations or save any edits to them, press the `Save` button. The final column in the spreadsheet contains the reason(s) for rejection. Each reason starts with `ERROR:` and occupies a single line. If reasons are added, please add them on an additional line of text in the spreadsheet cell. If an interview needs to be removed, right-click and select `Delete`.
- **Reject.** The application can reject all interviews contained in the file edited above. To execute this action, expand the `Reject` accordion and click on the `Run` button. Doing so will instruct the server to reject the interviews in the file and post the comments contained in the comment column.
- **Report.** NOTE: functionality not present yet; still under development 🚧

### Generate data quality report 📈

NOTE: functionality not present yet; still under development 🚧

</details>