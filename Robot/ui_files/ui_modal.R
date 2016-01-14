div( 
bsModal("id_modal_dataEntry", "Gather Your Data, Shape Your Future","dataEntry", size = "large",
        fluidRow(column(width=8,offset=2,
                        p("You will enter your farm data and your assumptions in this page."), 
                        p("The left column shows the categories of input items starting with", ems("Operation,"),
                          ems("Markets,"), "and so on. The items appear on the right with some default values.   
                          When you get to Capital, you will see the input items will be defined for three investmenet profiles,
                          labeled as",
                          ems("Robots,"), ems("Retrofit Parlors,"), "and", ems("New Parlors.")), 
                          p("Since there are many input items, we suggest that you get started by 
                          skimming the default values and make changes only if those values are drastically different 
                          from your situation."),
                          p("Also, please remember to download your input data using the", ems("Download"), 
                            "button at the bottom of the page. 
                          The downloaded file can be uploaded later to resume your analysis.")
        ))
),  

bsModal("id_modal_dashboardPanel", "Analyze Like an Executive","dashboardPanel", size = "large",
        fluidRow(column(width=8,offset=2,
                        p("You will quickly glance at the result using dashboard."), 
                          p("The dashbaord is your secretary who reports on the", em("changes"), "in",
                          ems("the income over feed cost (IOFC)"),  
                          "and", ems("Net Impact,"), "which consists of five categorical summaries of:", br(), 
                          "- Change in", ems("the Milk Income minus Feed Cost"),br(), 
                          "- Change in", ems("the Labor and Repiar Costs"),br(),
                          "- Change in", ems("the Cost of Capital"),br(),
                          "- Change associated with", ems("Other Budgetary Items"),br(),
                          "-", ems("Adjustments with Inflation"), "needed for a long investment span",br()
                          ), 
                        p("As you modify your data and assumptions,  
                          the dashbaord will quickly report new resutls. 
                          Like a good secretary, by reporting to you concisely and quickly,
                          it assists you on a complex business decision.") 
        ))
),  

bsModal("id_modal_partialBudget", "Divide and Conquer: Your Budget and Your Tax",
        "partialBudget", size = "large",
        fluidRow(column(width=8,offset=2,
                        p("You will see systematic changes in your operation 
                            through positive and negative influence on your budget 
                            and the associated changes in taxes."),
                          p("While the impact on budget may change slowly over time 
                            (at least in projection), the cash flows associated with
                          financing the investment and the tax deductions 
                          fluctuate more widely over time. To put all this in perspective,
                            we use the concept of annualized value, or
                            a constant annual payment or income that smoothes 
                            out wild fluctuations.")
        ))
        ), 

bsModal("id_modal_cashFlow", "Master the Flow of Cash: Tame and Ride","cashFlow", size = "large",
        fluidRow(column(width=8,offset=2,
                        p("Knowing the basics goes a long way."),
                        p("The better you can interpret the cash flow, 
                          the better you can scope out what lies ahead.
                           Moreover, having a clear understanding of your cash flow 
                           helps communicate your vision to your lender."),
                        p("The tables of", ems("Cash Flow,"), ems("Debt Calculation,"),
                          "and", ems("Depreciation"), "will equip you with an essential 
                          perspective on the cost of investment.") 
        ))
), 

bsModal("id_modal_summary", "Who is Going to Milk for You?","summary", size = "large",
        fluidRow(column(width=8,offset=2,
                        p("No matter how complicated your data and assumptions are, 
                          a decision can be reached if the analysis is summarized properly."),
                        p("Here you will see the summary of comparison among", 
                          ems("Robots,"), ems("Retrofit Parlors,"), "and", ems("New Parlors."),
                          "investment profiles.")
        ))
),  

bsModal("id_modal_sensitivityAnalysis", "Stay Focused, Stay Prepared: 
        Thought Experiments that Could Save Your Farm",
        "sensitivityAnalysis", size = "large",
        fluidRow(column(width=8,offset=2,
                        p("A long-term business decision must account for various possibilities 
                          that could drastically alter your calculation. "),
                        p("You will consider 10 key variables that may have major influence 
                          on your result.")
        ))
) 
)



