       identification division.
       program-id. DataSplitCount.
       author. Joree Miranda, Kyle Bayer, Ashante Smith
       date-written. 2018-04-20
      *This is our 2nd program for our MAFD final project. It will split
      *returns into one file, sales and layaways into another file.
      *It will then count and calculate totals.

       environment division.
       configuration section.
       input-output section.

       file-control.
      
      * input-file declaration
           select input-file
           assign to "../../../data/valid.dat"
           organization is line sequential.

      * output file declarations
           select sale-layaways-file
           assign to "../../../data/salelayaways.dat"
           organization is line sequential.

           select returns-file
           assign to "../../../data/returns.dat"
           organization is line sequential. 

           select report-file
           assign to "../../../data/Prog2Report.out"
           organization is line sequential.
       

       data division.
       file section.

       fd input-file
           data record is input-line
           record contains 36 characters.

       01 input-line.
           05 il-transaction-code         pic X.
           05 il-transaction-amount       pic 9(5)V99.
           05 il-payment-type             pic XX.
           05 il-store-number             pic XX.
           05 il-invoice-number           pic X(9).
           05 il-sku-code                 pic X(15).

       fd sale-layaways-file
           data record is sale-layaways-line
           record contains 36 characters.

       01 sale-layaways-line              pic x(36).

       fd returns-file
           data record is returns-line
           record contains 36 characters.

       01 returns-line                    pic x(36).

       fd report-file
           data record is report-line
           record contains 36 characters.

       01 report-line                      pic x(36).

       working-storage section.

       01 ws-transaction-code             pic x
           value spaces.

       01 ws-layaways-amount              pic 9(6)v99
           value 0.

       01 ws-sales-amount                 pic 9(6)v99
           value 0.

       01 ws-returns-amount               pic 9(6)v99
           value 0.

       01 ws-grand-total                  pic 9(6)v99
           value 0.

       01 ws-total-amount                 pic 9(6)v99
           value 0.

       01 ws-total-layaways-amount-detail.
           05 filler                      pic x(24)
               value "TOTAL LAYAWAYS AMOUNT: ".
           05 ws-layaways-amount-output   pic $z9,999.99
               value 0.

       01 ws-total-sale-amount-detail.
           05 filler                      pic x(24)
               value "TOTAL SALES AMOUNT: ".
           05 ws-sales-amount-output      pic $99,999.99
               value 0.

       01 ws-total-amount-detail.
           05 filler                      pic x(24)
               value "TOTAL S&L AMOUNT: ".
           05 ws-total-amount-output      pic $99,999.99
               value 0.

       01 ws-total-return-amount-detail.
           05 filler                      pic x(24)
               value "TOTAL RETURNS AMOUNT: ".
           05 ws-returns-amount-output    pic $zz,999.99
               value 0.

       01 ws-grand-total-detail.
           05 filler                      pic x(24)
               value "GRAND TOTAL: ".
           05 ws-grand-total-output       pic $zz,999.99

       01 ws-total-sale-layaways-detail.
           05 filler                      pic x(24)
               value "TOTAL SALES & LAYAWAYS: ".
           05 ws-total-sale-layaways      pic 9(3)
               value 0.

       01 ws-total-returns-detail.           
           05 filler                      pic x(25)
               value "TOTAL RETURNS: ".
           05 ws-total-returns            pic 9(2)
               value 0.

       01 ws-total-sale-detail.
           05 filler                      pic x(25)
               value "TOTAL SALES: ".
           05 ws-total-sales              pic 9(2)
               value 0.

       01 ws-total-layaways-detail.
           05 filler                      pic x(25)
               value "TOTAL LAYAWAYS: ".
           05 ws-total-layaways           pic 9(2)
               value 0.

       01 ws-transaction-total-detail.              
           05 filler                      pic x(24)
               value "TOTAL TRANSACTIONS: ".
           05 ws-total-transactions       pic 9(3)
               value 0.

       01 ws-sales-percentage-detail.
           05 filler                      pic x(24)
               value "SALES PERCENTAGE: ".
           05 ws-sales-percentage         pic V99
               value 0.
           05 filler                      pic X
               value "%".

       01 ws-layaways-percentage-detail.
           05 filler                      pic x(24)
               value "LAYAWAYS PERCENTAGE: ".
           05 ws-layaways-percentage      pic V99
               value 0.
           05 filler                      pic X
               value "%".



       01 ws-eof-flag                  pic x
           value "N".

       01 ws-transaction-code-flag     pic x
           value "N".

       77 ws-sale-code                 pic x
           value "S".

       77 ws-layaway-code              pic x
           value "L".

       77 ws-return-code              pic x
           value "R".


       procedure division.
      * open files
           open input input-file.
           open output sale-layaways-file returns-file report-file.
       
      * read initial record from input-file
           read input-file at end move "Y" to ws-eof-flag.
           
           perform until ws-eof-flag = 'Y'
               perform 00-main
           end-perform.
           
           perform 40-output-totals.
           close input-file sale-layaways-file returns-file report-file.
           
           goback.

       00-main.
           perform 20-process-lines.
           perform 30-split-files.
           perform 35-count.
           perform 37-calculate-totals.
           perform 50-calculate-percentage.

           read input-file at end move 'Y' to ws-eof-flag.

       
       20-process-lines.
           move il-transaction-code to ws-transaction-code.

       
       30-split-files.
           if (ws-transaction-code = ws-sale-code OR
               ws-transaction-code = ws-layaway-code) then
               write sale-layaways-line from input-line
           else if (ws-transaction-code = ws-return-code) then
                write returns-line from input-line
                end-if
           end-if.

       35-count.
           if (ws-transaction-code = ws-sale-code)
               add 1 to ws-total-sales
               add 1 to ws-total-sale-layaways
               add il-transaction-amount to ws-sales-amount
           else if(ws-transaction-code = ws-layaway-code) then
                    add 1 to ws-total-layaways
                    add 1 to ws-total-sale-layaways
                    add il-transaction-amount to ws-layaways-amount
                else if(ws-transaction-code = ws-return-code) then
                         add 1 to ws-total-returns
                         add il-transaction-amount to ws-returns-amount
                     end-if
                end-if
           end-if.


       37-calculate-totals.
           
           add ws-total-returns to ws-total-sale-layaways giving
               ws-total-transactions.
           add ws-layaways-amount ws-sales-amount giving 
               ws-total-amount.

           subtract ws-returns-amount from ws-total-amount giving
               ws-grand-total.

           move ws-total-amount to ws-total-amount-output.
           move ws-grand-total to ws-grand-total-output.
           move ws-returns-amount to ws-returns-amount-output.
           move ws-layaways-amount to ws-layaways-amount-output.
           move ws-sales-amount to ws-sales-amount-output.

       40-output-totals.
           write report-line from ws-total-sale-layaways-detail
           write report-line from ws-total-sale-detail.
           write report-line from ws-total-layaways-detail.
           write report-line from ws-total-returns-detail.
           write report-line from ws-transaction-total-detail.

           write report-line               
               from ws-total-amount-detail after advancing 1 line.
           write report-line
               from ws-total-sale-amount-detail.
           write report-line
               from ws-total-layaways-amount-detail.
           write report-line
               from ws-total-return-amount-detail.
           write report-line
               from ws-grand-total-detail.


           write report-line from ws-layaways-percentage-detail
               after advancing 1 line.
           write report-line from ws-sales-percentage-detail.

       50-calculate-percentage.
           compute ws-layaways-percentage rounded = 
               ws-total-layaways / ws-total-sale-layaways.

           compute ws-sales-percentage rounded =
               ws-total-sales / ws-total-sale-layaways.
       end program DataSplitCount.