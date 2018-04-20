       identification division.
       program-id. SLProcessingProgram.
       author. Joree Miranda, Kyle Bayer, Ashante Smith
       date-written. 2018-04-19
      *This is our 3rd program for our MAFD 4201 Final Project. It will
      *output all of the sales and layaways transaction into a detailed
      *report.

       environment division.
       configuration section.

       file-control.

      * input-file declaration
           select input-file
           assign to "../../../data/salelayaways.dat"
           organization is line sequential.

      * output file declarations
           select output-file
           assign to "../../../data/Prog3Report.out"
           organization is line sequential.

       data division.
       file section.

       fd input-file
           data record is input-line
           record contains 36 characters.

       01 input-line.
           05 il-transaction-code      pic X.
           05 il-transaction-amount    pic 9(5)V99.
           05 il-payment-type          pic XX.
           05 il-store-number          pic XX.
           05 il-invoice-number        pic X(9).
           05 il-sku-code              pic X(15).

       fd output-file
           data record is print-line
           record contains 87 characters.

       01 print-line                   pic x(87).

       working-storage section.

       01 ws-title.
           05 ws-date                  pic 9(6)
               value 0.
           05 filler                   pic x(5)
               value spaces.
           05 ws-time                  pic 9(8)
               value 0.
           05 filler                   pic x(12)
               value spaces.
           05 filler                   pic x(24)
               value "SALES & LAYAWAYS REPORT".
           05 filler                   pic x(12)
               value spaces.
           05 filler                   pic x(7)
               value "GROUP 3".

       01 ws-heading.
           05 filler                   pic x(11)
               value "TRANSACTION".
           05 filler                   pic x(4)
               value spaces.
           05 filler                   pic x(6)
               value "AMOUNT".
           05 filler                   pic x(7)
               value spaces.
           05 filler                   pic x(3)
               value "TAX".
           05 filler                   pic x(6)
               value spaces.
           05 filler                   pic x(12)
               value "PAYMENT TYPE".
           05 filler                   pic x(3)
               value spaces.
           05 filler                   pic x(7)
               value "STORE #".
           05 filler                   pic x(3)
               value spaces.
           05 filler                   pic x(9)
               value "INVOICE #".
           05 filler                   pic x(3)
               value spaces.
           05 filler                   pic x(8)
               value "SKU CODE".

       01 ws-page-title.
           05 filler                   pic x(40)
               value "----------------------------------------".
           05 filler                   pic x(4)
               value "PAGE".
           05 filler                   pic x(1)
               value spaces.
           05 ws-page-count            pic 9
               value 0.
           05 filler                   pic x(41)
               value "-----------------------------------------".

       01 ws-report-output.
           05 ws-transaction-output    pic x(7)
               value spaces.
           05 filler                   pic x(5)
               value spaces.
           05 ws-transaction-amount    pic $zz,zz9.99
               value 0.
           05 filler                   pic x(3)
               value spaces.
           05 ws-tax-output            pic $zz,zz9.99
               value 0.
           05 filler                   pic x(5)
               value spaces.
           05 ws-payment-output        pic X(6)
               value spaces.
           05 filler                   pic x(8)
               value spaces.
           05 ws-store-number          pic XX
               value spaces.
           05 filler                   pic x(6)
               value spaces.
           05 ws-invoice-number        pic X(9)
               value spaces.
           05 filler                   pic x(3)
               value spaces.
           05 ws-sku-code              pic X(15)
               value spaces.


       01 ws-eof-flag                  pic X
           value 'N'.

       01 ws-total-sale-detail.
           05 filler                   pic x(24)
               value "SALE TRANSACTIONS: ".
           05 ws-total-sales           pic 9(2)
               value 0.

       01 ws-total-layaways-detail.
           05 filler                   pic x(24)
               value "LAYAWAY TRANSACTIONS: ".
           05 ws-total-layaways        pic 9(2)
               value 0.

       01 ws-tax                       pic 9999v99
           value 0.

       01 ws-total-cash                pic 99
           value 0.

       01 ws-total-credit              pic 99
           value 0.

       01 ws-total-debit               pic 99
           value 0.

       01 ws-total-tax                 pic 9999v99
           value 0.

       01 ws-total-amount              pic 9(6)v99
           value 0.

       01 ws-layaways-amount           pic 9(6)v99
           value 0.

       01 ws-sales-amount              pic 9(6)v99
           value 0.

       01 ws-total-sale-layaways       pic 999
           value 0.

       01 ws-payment-type              pic x(2)
           value spaces.

       01 ws-transaction-code          pic X
           value spaces.

       01 ws-cash-percentage-detail.
           05 filler                   pic x(24)
               value "CASH PERCENTAGE: ".
           05 ws-cash-percentage       pic V99
               value 0.
           05 filler                   pic X
               value "%".

       01 ws-credit-percentage-detail.
           05 filler                   pic x(24)
               value "CREDIT PERCENTAGE: ".
           05 ws-credit-percentage     pic V99
               value 0.
           05 filler                   pic x
               value "%".

       01 ws-debit-percentage-detail.
           05 filler                   pic x(24)
               value "DEBIT PERCENTAGE: ".
           05 ws-debit-percentage      pic V99
               value 0.
           05 filler                   pic x
               value "%".

       01 ws-total-tax-detail.
           05 filler                   pic x(24)
               value "TOTAL TAX: ".
           05 ws-total-tax-output      pic $z9,999.99
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
               value "TOTAL AMOUNT: ".
           05 ws-total-amount-output      pic $99,999.99
               value 0.



       77 ws-cash-code                 pic xx
           value "CA".

       77 ws-credit-code               pic xx
           value "CR".

       77 ws-debit-code                pic xx
           value "DB".

       77 ws-line-count                pic 99
           value 0.

       77 ws-lines-per-page            pic 99
           value 20.

       77 ws-sale-transaction-code     pic x
           value "S".

       77 ws-layaway-transaction-code  pic x
           value "L".

       77 ws-tax-percent               pic 9v99
           value 0.13.

       77 ws-sale-code                 pic x
           value 'S'.

       77 ws-layaway-code              pic x
           value "L".

       procedure division.
           open input input-file.
           open output output-file.

           read input-file at end move 'Y' to ws-eof-flag.
           accept ws-date from date.
           accept ws-time from time.

           write print-line from ws-title.

           perform until ws-eof-flag = 'Y'
               add 1 to ws-page-count
               write print-line from ws-page-title
               write print-line from ws-heading

               perform 00-main
                   varying ws-line-count
                   from 1 by 1
                   until (ws-eof-flag = 'Y'
                       OR ws-line-count > ws-lines-per-page)
           end-perform.
           perform 40-output-totals

           close input-file output-file.

           goback.
           
       00-main.
           perform 10-process-lines.
           perform 11-format-payment.
           perform 12-format-transaction-code.
           perform 15-calculate-tax.
           perform 20-write-records.
           perform 30-count.
           perform 32-calculate-percentage.
           perform 37-calculate-totals.

           read input-file at end move 'Y' to ws-eof-flag.

       10-process-lines.
           move il-transaction-code to ws-transaction-code.
           move il-transaction-amount to ws-transaction-amount.
           move il-payment-type to ws-payment-type.
           move il-store-number to ws-store-number.
           move il-invoice-number to ws-invoice-number.
           move il-sku-code to ws-sku-code.

       11-format-payment.
           if (ws-payment-type = ws-cash-code) then
               move "CASH" to ws-payment-output
           else if(ws-payment-type = ws-credit-code) then
                    move "CREDIT" to ws-payment-output
                else if(ws-payment-type = ws-debit-code) then
                         move "DEBIT" to ws-payment-output
                     end-if
                end-if
           end-if.

       12-format-transaction-code.
           if (ws-transaction-code = ws-sale-code) then
               move "SALE" to ws-transaction-output
           else if (ws-transaction-code = ws-layaway-code) then
                    move "LAYAWAY" to ws-transaction-output
                end-if
           end-if.

       15-calculate-tax.
           compute ws-tax rounded = 
               il-transaction-amount * ws-tax-percent.

           move ws-tax to ws-tax-output.


       20-write-records.
           write print-line from ws-report-output
               after advancing 1 line.

       30-count.
           if (ws-transaction-code = ws-sale-transaction-code)
               add 1 to ws-total-sales
               add 1 to ws-total-sale-layaways
               add il-transaction-amount to ws-sales-amount
           else if(ws-transaction-code = ws-layaway-transaction-code)
               add 1 to ws-total-layaways
               add 1 to ws-total-sale-layaways
               add il-transaction-amount to ws-layaways-amount
               end-if
           end-if.

           if (ws-payment-type = ws-cash-code) then
               add 1 to ws-total-cash
           else if (ws-payment-type = ws-credit-code) then
                    add 1 to ws-total-credit
                else if (ws-payment-type = ws-debit-code) then
                         add 1 to ws-total-debit
                     end-if
                end-if
           end-if.

       32-calculate-percentage.
           compute ws-cash-percentage rounded =
               ws-total-cash / ws-total-sale-layaways.

           compute ws-credit-percentage rounded =
               ws-total-credit / ws-total-sale-layaways.

           compute ws-debit-percentage rounded =
               ws-total-debit / ws-total-sale-layaways.



       37-calculate-totals.
           
           add ws-layaways-amount ws-sales-amount giving 
               ws-total-amount.
           
           multiply ws-total-amount by ws-tax-percent giving
               ws-total-tax rounded.

           move ws-total-tax to ws-total-tax-output.
           move ws-total-amount to ws-total-amount-output.
           move ws-layaways-amount to ws-layaways-amount-output.
           move ws-sales-amount to ws-sales-amount-output.
           



       40-output-totals.
           write print-line from ws-total-layaways-detail
               after advancing 2 lines.
           write print-line from ws-total-sale-detail.

           write print-line from ws-cash-percentage-detail
               after advancing 1 line.
           write print-line from ws-credit-percentage-detail.
           write print-line from ws-debit-percentage-detail.

           write print-line               
               from ws-total-amount-detail after advancing 1 line.
           write print-line
               from ws-total-sale-amount-detail.
           write print-line
               from ws-total-layaways-amount-detail.
           write print-line from ws-total-tax-detail

       end program SLProcessingProgram.