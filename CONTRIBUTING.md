# Contributing to CRI-iAtlas

Welcome, and thanks for your interest in contributing to the Cancer Research Institute (CRI) iAtlas!

If you would like to contribute code changes please follow these steps:

1. Search the issues list in the repo. Consider opening an issue related to the suggested feature.
2. Create a personal fork and commit new code development to personal branches in that fork.
3. Propose changes via pull request of personal branches.


In case the intended contribution consists of a new module, please refer to the following steps to include shortcuts of new modules in the Explore Section of CRI-iAtlas.

## Procedures for Starting a New iAtlas Module

Procedures to include a new module in the initial page of the Explore Section of the current version of the portal CRI-iAtlas (February 2020). 

This procedure lists the files that need to be updated or started, as well as the functions to call, when applicable. There is no reference to a specific line to update, as this can change over time. It is recommended to look for the section of the code with the settings of current modules - for example, in step 4.1., at the `server.R` file, look for `callModule` code for the existing modules, and include new code in this section.

### Steps
(Can be carried out in another order but pieces need to be in place for full functionality. As true in general, begin with a new branch) 

#### 1. Create file with new module and save it in the modules folder : `modules/myshinynewmodule.R`.

#### 2. Edit `configuration.yaml`.

Add the new module name in the `module_files` list: `modules/myshinynewmodule.R`

IMPORTANT: if you create a script with functions, save it in the `functions` folder and add the file names in the `function_files` list in `configuration.yaml`

#### 3. In your module script `myshinynewmodule.R`, define the name of the UI and server function. 

In this example, the names will be: 

```
newmodule_UI <-function(id){}

newmodule <- function(input, output, session){}
```
Please note that this function can take more arguments.
 
#### 4. Edit top level `server.R`.

Include a `callModule` for your module server function:

```
callModule(newmodule, moduleN, …)
```

Give module a number such as `moduleN`. Check if moduleN is unique.

Add the following in the `observeEvent` section:
```
    observeEvent(input$link_to_moduleN, {
      shinydashboard::updateTabItems(session, "explorertabs", "new_module")
    })
```
Observe that in 4.2., "new_module” is distinct from newmodule defined in #3.

#### 5. Now it is time to include access to your new module in the initial page of the portal. To do so, edit `pages/explorepage.R`.

Add the following in the `tabItem` section found near bottom of this script:

```
tabItem(
        tabName = "new_module",
        newmodule_UI("moduleN")
      )
```

Add link to the left navigation bar (into code near the top of this script):

```
 menuSubItem(
                 "Test Module", #text to display
                 tabName = "new_module",
                 icon = icon("cog") #can be different for other sections
      )
```

Edit main section to include the new module in the initial Explore page:
```
fluidRow(
            imgLinkBox(
              width = 6,
              title = "New Module",
              linkId = "link_to_moduleN",
              imgSrc = "images/myimage.png", # if including images, read next step.
              boxText = "Short description of the module",
              linkText = "Open Module"
)
```
You may have to adjust layout after step 5.3.
 
Include a png if you like. Save the image at `www/images/myimage.png`.

