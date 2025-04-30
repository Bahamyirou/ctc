# Shiny app for CTC

# <img src="img/logo4.jpg" align="right" alt="" width="200"/> 

A RShiny-based dashboard for monitoring, analyzing the data of CTC

## 🚀 Features

- 📊 View distribution of Togolese community in Canada
- 📈 View memebership Trend
- 📊 View section specific need


## 🏗️ Architecture

```
CTC-Shiny-App/
├── app.py                    # Main application entry
├── views/                    # Page components
│   ├── large-jumps.py        # Handles the "Large Jumps" page
│   ├── latest-measures.py    # Handles the "Latest Measures" page
│   ├── mpox.py               # Handles the "Mpox Trends" page
│   ├── ww-trends.py          # Handles the "Wastewater Trends" page
├── utils.py                  # Shared util functions
├── .env                      # Environment configuration
└── requirements.txt          # Dependencies
```

## 🛠️ Where it is published

```bash
the app is hosted in https://www.shinyapps.io/. The app link is the following:
```
