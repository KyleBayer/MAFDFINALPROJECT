       identification division.
       program-id. DataSplitCount.

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
           assign to "../../../data/salelayaways.out"
           organization is line sequential.

           select returns-file
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

       fd sale-layaways-file
           data record is sale-layaways-line
           record contains 36 characters.

       01 sale-layaways-line           pic x(36).

       fd returns-file
           data record is returns-line
           record contains 36 characters.

       01 returns-line                 pic x(36).

       working-storage section.

       01 ws-transaction-code          pic x
           value spaces.

       01 ws-total-sale-layaways-detail.
           05 filler                   pic x(24)
               value "Total Sales & Layaways: ".
           05 ws-total-sale-layaways   pic 9(3)
               value 0.

       01 ws-total-returns-detail.           
           05 filler                   pic x(25)
               value "Total Returns: ".
           05 ws-total-returns         pic 9(2)
               value 0.

       01 ws-total-sale-detail.
           05 filler                   pic x(25)
               value "Total Sales: ".
           05 ws-total-sales           pic 9(2)
               value 0.

       01 ws-total-layaways-detail.
           05 filler                   pic x(25)
               value "Total Layaways: ".
           05 ws-total-layaways        pic 9(2)
               value 0.

       01 ws-grand-total-detail.              
           05 filler                   pic x(24)
               value "Grand Total: ".
           05 ws-grand-total           pic 9(3)
               value 0.

       01 ws-sales-percentage-detail.
           05 filler                   pic x(24)
               value "Sales Percentage: ".
           05 ws-sales-percentage      pic V99
               value 0.
           05 filler                   pic X
               value "%".

       01 ws-layaways-percentage-detail.
           05 filler                   pic x(24)
               value "Layaways Percentage: ".
           05 ws-layaways-percentage   pic V99
               value 0.
           05 filler                   pic X
               value "%".



       01 ws-eof-flag                  pic x
           value "N".

       01 ws-transaction-code-flag     pic x
           value "N".

       77 ws-sale-transaction-code     pic x
           value "S".

       77 ws-layaway-transaction-code  pic x
           value "L".

       77 ws-returns-transaction-code  pic x
           value "R".


       procedure division.
      * open files
           open input input-file.
           open output sale-layaways-file returns-file.
       
      * read initial record from input-file
           read input-file at end move "Y" to ws-eof-flag.
           
           perform until ws-eof-flag = 'Y'
               perform 00-main
           end-perform.
           
           perform 40-output-totals.
           close input-file sale-layaways-file returns-file.
           
           goback.

       00-main.
           perform 20-process-lines.
           perform 30-split-files.
           perform 35-count.
           perform 50-calculate-percentage.

           read input-file at end move 'Y' to ws-eof-flag.

       
       20-process-lines.
           move il-transaction-code to ws-transaction-code.
       
       30-split-files.
           if (ws-transaction-code = ws-sale-transaction-code OR
               ws-transaction-code = ws-layaway-transaction-code) then
               add 1 to ws-total-sale-layaways
               write sale-layaways-line from input-line
           else if (ws-transaction-code
                       = ws-returns-transaction-code) then
                add 1 to ws-total-returns
               write returns-line from input-line
           end-if.

       35-count.
           if (ws-transaction-code = ws-sale-transaction-code)
               add 1 to ws-total-sales
           else if(ws-transaction-code = ws-layaway-transaction-code)
               add 1 to ws-total-layaways
           end-if.

           add ws-total-returns to ws-total-sale-layaways giving
             ws-grand-total.
       40-output-totals.
           write sale-layaways-line from ws-total-sale-layaways-detail
               after advancing 1 line.
           write sale-layaways-line from ws-total-sale-detail.
           write sale-layaways-line from ws-total-layaways-detail.
           write returns-line from ws-total-returns-detail
               after advancing 1 line.
           write sale-layaways-line from ws-grand-total-detail.
           write returns-line from ws-grand-total-detail.
           write sale-layaways-line from ws-layaways-percentage-detail
               after advancing 1 line.
           write sale-layaways-line from ws-sales-percentage-detail.
          
       50-calculate-percentage.
           compute ws-layaways-percentage rounded = 
               ws-total-layaways / ws-total-sale-layaways.

           compute ws-sales-percentage rounded =
               ws-total-sales / ws-total-sale-layaways.
       end program DataSplitCount.