       identification division.
       program-id. DataSplitCount.

       environment division.
       configuration section.
       input-output section.

       file-control.
      
      * input-file declaration
           select valid-file
           assign to "../../EDITS/data/valid.out"
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

       fd valid-file
           data record is valid-line
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

       01 sale-layaways                pic x(36).

       fd returns-file
           data record is sale-layaways-line
           record contains 36 characters.

       01 returns                      pic x(36).

       working-storage section.

       77 ws-eof-flag                  pic x
           value "N".

       77 ws-transaction-code-flag     pic x
           value "N".

       procedure division.
           00-main.
      * open files
           open input valid-file.
           open output sale-layaways-file returns-file.
       
      * read initial record from input-file
           read valid-file at end move "Y" to ws-eof-flag.
           
      * iterate through all input lines        
           perform 20-process-lines until ws-eof-flag = "Y".
      * write totals
      *    perform 50-output-totals.
      * close files
           close valid-file sale-layaways-file returns-file.
           
           goback.

       20-process-lines.
           perform until ws-eof-flag = "Y"

           if(il-transaction-code is not equal to "S") then
               if(il-transaction-code is not equal to "L")
                   move "N" to ws-transaction-code-flag
               else
                   move "Y" to ws-transaction-code-flag
               end-if
           else 
               move "Y" to ws-transaction-code-flag
           end-if

           if(ws-transaction-code-flag equals "N") then
               perform 30-output-returns-record
           else if(ws-transaction-code-flag equals "Y") then
               perform 40-output-sl-record
           end-if
      * read next input-file record
           read valid-file at end move "Y" to ws-eof-flag

           end-perform. 

       30-output-returns-record.
           write returns from input-line.

       40-output-sl-record.
           write sale-layaways from input-line.
       end program DataSplitCount.