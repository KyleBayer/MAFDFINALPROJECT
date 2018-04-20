       identification division.
       program-id. TypeRProcessing.
       author. Joree Miranda, Kyle Bayer, Ashante Smith
       date-written. 2018-04-19
      * This is the fourth program file created as part of our 
      * MAFD 4201 Final Project. The purpose of this program is to
      * produce a detail report of all returns.
        

       environment division.
       configuration section.
       input-output section.

       file-control.
      
      * input-file declaration
           select input-file
           assign to "../../../data/returns.dat"
           organization is line sequential.

           select output-file
           assign to "../../../data/returns.out"
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
           data record is output-line
           record contains 36 characters.
       
       01 print-line                   pic x(75).

       working-storage section.

       01 ws-output-line.
           05 filler                   pic X(2)
               value spaces.
           05 ol-transaction-code      pic X.
           05 filler                   pic X(2)
               value spaces.
           05 ol-transaction-amount    pic $99.99.
           05 filler                   pic X(2)
               value spaces.
           05 ol-payment-type          pic XX.
           05 filler                   pic X(3)
               value spaces.
           05 ol-store-number          pic XX.
           05 filler                   pic X(4)
               value spaces.
           05 ol-invoice-number        pic X(9).
           05 filler                   pic X(2)
               value spaces.
           05 ol-sku-code              pic X(15).

       01 ws-eof-flag                  pic x
           value "N".
       
       01 ws-line-break                pic x(36)
           value spaces.

       01 ws-report-heading.
           05 filler                   pic x(16)
               value spaces.
           05 ws-title                 pic x(17)
               value "Type R Processing".

       01 ws-header.
             
           05 filler                   pic x(4)
               value "Code".
           05 filler                   pic x(2)
               value spaces.
           05 filler                   pic x(6)
               value "Amount".
           05 filler                   pic x(1)
               value spaces.
           05 filler                   pic x(4)
               value "Type".
           05 filler                   pic x(1)
               value spaces.
           05 filler                   pic x(6)
               value "Store#".
           05 filler                   pic x(1)
               value spaces.
           05 filler                   pic x(8)
               value "Invoice#".
           05 filler                   pic x(5)
               value spaces.
           05 filler                   pic x(8)
               value "SKU Code".

       01 ws-page-title.
           05 filler                   pic x(22)
               value "----------------------".
           05 filler                   pic x(4)
               value "PAGE".
           05 filler                   pic x(1)
               value spaces.
           05 ws-page-count            pic 9
               value 0.
           05 filler                   pic x(23)
               value "-----------------------".

       01 ws-number-records.
           05 filler                   pic x(19)
               value "Number of Records: ".
           05 ws-record-count          pic 99
               value 0.

       01 ws-tax-owed.
           05 filler                   pic x(11)
               value "Tax owed: ".
           05 ws-tax-total             pic $99.99.
       
       77 ws-tax-temp                  pic 9(5)V99.
       77 ws-lines-per-page            pic 99
           value 20.
       77 ws-line-count                pic 99
           value 0.
       77 ws-13-percent                pic 9(9)v99
           value 0.13.
       


       procedure division.
      * open files
           open input input-file.
           open output output-file.
       
      * Output header
           
      * read initial record from input-file
           read input-file at end move "Y" to ws-eof-flag.
           
           write print-line from ws-report-heading

           write print-line from ws-line-break

           perform 20-output-header

           perform until ws-eof-flag = 'Y'
               add 1 to ws-page-count
               write print-line from ws-page-title
               perform 00-main-logic
               varying ws-line-count
               from 1 by 1
               until (ws-eof-flag = 'Y'
                      OR ws-line-count > ws-lines-per-page)

           write print-line from ws-number-records

           write print-line from ws-tax-owed

           end-perform.
           
           
           close input-file output-file.

           goback.

       00-main-logic.
           perform 30-processing-data
           perform 40-write-output
           add il-transaction-amount to ws-tax-temp
           perform 50-calculate-tax
           read input-file at end move 'Y' to ws-eof-flag.

       20-output-header.
           write print-line from ws-header
           write print-line from ws-line-break.

      * Moves input line to output line.
       30-processing-data.
           move il-invoice-number to ol-invoice-number
           move il-payment-type to ol-payment-type
           move il-sku-code to ol-sku-code
           move il-store-number to ol-store-number
           move il-transaction-amount to ol-transaction-amount
           move il-transaction-code to ol-transaction-code.

       40-write-output.
           add 1 to ws-record-count
           write print-line from ws-output-line.
           

       50-calculate-tax.
           multiply ws-tax-temp by ws-13-percent giving ws-tax-total.
       end program TypeRProcessing.

       
           