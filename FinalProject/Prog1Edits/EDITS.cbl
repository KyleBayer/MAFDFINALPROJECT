       identification division.
       program-id. EDITS.
       author. Joree Miranda, Kyle Bayer, Ashante Smith
       date-written. 2018-04-20
      *This is our 1st program for our mafd final project. It will 
      *read an data file, and sort which record is valid, and which is
      *invalid. It will also make a report which show the record that is
      *invalid and show what is wrong with the record.

       environment division.
       configuration section.
       input-output section.

       file-control.
      
      * input-file declaration
           select input-file
           assign to "../../../data/project1.dat"
           organization is line sequential.

      * output file declarations
           select valid-file
           assign to "../../../data/valid.dat"
           organization is line sequential.

           select invalid-file
           assign to "../../../data/invalid.dat"
           organization is line sequential.

           select report-file
           assign to "../../../data/Prog1Report.out"
           organization is line sequential.

       data division.
       file section.

       fd input-file
           data record is input-file
           record contains 36 characters.

       01 input-line.
           05 il-transaction-code      pic X.
           05 il-transaction-amount    pic 9(5)V99.
           05 il-payment-type          pic XX.
           05 il-store-number          pic XX.
           05 il-invoice-number        pic X(9).
           05 il-sku-code              pic X(15).

       fd valid-file
           data record is valid-line
           record contains 36 characters.
      * declare valid line
       01 valid-line                   pic x(36).

       fd invalid-file
           data record is invalid-line
           record contains 36 characters.
      * declare invalid line
       01 invalid-line                 pic x(36).

       fd report-file
           data record is report-line
           record contains 40 characters.
       01 report-line                  pic x(40).

       working-storage section.

       01 ws-title.
           05 ws-date                  pic 9(6)
               value 0.
           05 filler                   pic x(5)
               value spaces.
           05 ws-time                  pic 9(8)
               value 0.

       01 ws-title-line-2.
           05 filler                   pic x(11)
               value "EDIT REPORT".

       01 ws-name-line.
           05 filler                   pic x(40)
               value "KYLE BAYER, JOREE MIRANDA, ASHANTE SMITH".

       01 ws-line-break                pic x(36)
           value spaces.

       01 ws-error-message.
           05 ws-heading               pic x(7)
               value "Errors:".
           05 filler                   pic x(2)
               value spaces.
           05 ws-transaction-code      pic X
               value spaces.
           05 filler                   pic x(2)
               value spaces.
           05 ws-transaction-amt       pic 9(5).
           05 filler                   pic x(2)
               value spaces.
           05 ws-payment-type          pic XX
               value spaces.
           05 filler                   pic x(2)
               value spaces.
           05 ws-store-number          pic XX
               value spaces.
           05 filler                   pic x(2)
               value spaces.
           05 ws-sku-code              pic X(15)
               value spaces.

       01 ws-total-valid.
           05 filler                   pic x(15)
               value "Valid Records: ".
           05 ws-valid                 pic 9(3)
               value 0.

       01 ws-total.
           05 filler                   pic x(15)
               value "Total Records: ".
           05 ws-total-output          pic 9(3)
               value 0.
           

       01 ws-total-invalid.
           05 filler                   pic x(17)
               value "Invalid Records: ".
           05 ws-invalid               pic 9(2)
               value 0.

       77 ws-eof-flag                  pic x
           value "N".
       77 ws-transaction-code-flag     pic x
           value "N".
       77 ws-transaction-amt-flag      pic x
           value "N".
       77 ws-payment-type-flag         pic x
           value "N".
       77 ws-store-number-flag         pic x
           value "N".
       77 ws-spaces-flag               pic x
           value "N".

       procedure division.

       00-main.
      * open files
           open input input-file.
           open output valid-file invalid-file report-file.
       
      * read initial record from input-file
           read input-file at end move "Y" to ws-eof-flag.
           accept ws-date from date.
           accept ws-time from time.

           write report-line from ws-title.
           write report-line from ws-title-line-2.
           write report-line from ws-name-line.
           
      * iterate through all input lines        
           perform 20-process-lines until ws-eof-flag = "Y".

           perform 45-calculate-totals.
      * write totals
           perform 50-output-totals.
      * close files
           close input-file valid-file invalid-file report-file.
           
           goback.

       20-process-lines.
           
      *TODO: determine if line is valid

      * Checks if records are "S", "R" or "L"
           perform until ws-eof-flag equals "Y"
               
           if(il-transaction-code is not equal to "S")
               if(il-transaction-code is not equal to "R")
                   if(il-transaction-code is not equal to "L")
                        move "N" to ws-transaction-code-flag
                        move il-transaction-code to ws-transaction-code

                   else
                       move "Y" to ws-transaction-code-flag
                   end-if
               else 
                   move "Y" to ws-transaction-code-flag
               end-if
           else
               move "Y" to ws-transaction-code-flag
           end-if

      * Checks if transaction amount is numeric
           if(il-transaction-amount is numeric) then
               move "Y" to ws-transaction-amt-flag
           else
               move "N" to ws-transaction-amt-flag
               move il-transaction-amount to ws-transaction-amt
           end-if

      * Checks if payment type is ‘CA’, ‘CR’ or ‘DB’
           if(il-payment-type is not equal to "CA")
               if(il-payment-type is not equal to "CR")
                   if(il-payment-type is not equal to "DB")
                        move "N" to ws-payment-type-flag
                        move il-payment-type to ws-payment-type
                   else
                       move "Y" to ws-payment-type-flag
                   end-if
               else 
                   move "Y" to ws-payment-type-flag
               end-if
           else
               move "Y" to ws-payment-type-flag
           end-if

      * Checks if store number is ‘01’, 02’, ‘03’ or ‘07’
          if(il-store-number is not equal to 07)
           if(il-store-number is not equal to 01)
               if(il-store-number is not equal to 02)
                   if(il-store-number is not equal to 03)
                        move "N" to ws-store-number-flag
                        move il-store-number to ws-store-number
                   else
                       move "Y" to ws-store-number-flag
                   end-if
               else 
                   move "Y" to ws-store-number-flag
               end-if
           else
               move "Y" to ws-store-number-flag
           end-if
          else 
              move "Y" to ws-store-number-flag
           end-if
          
      * Error validation for empty spaces
           if(il-sku-code is equal to spaces) then
               move "N" to ws-spaces-flag
               move il-sku-code to ws-sku-code
           else
               move "Y" to ws-spaces-flag
           end-if
      *TODO: output record to appropriate file based on results of validaiton processing
        if (ws-spaces-flag is equal to "Y") then
           if (ws-payment-type-flag is equal to "Y") then
               if(ws-store-number-flag is equal to "Y") then
                   if(ws-transaction-amt-flag is equal to "Y") then
                       if(ws-transaction-code-flag is equal to "Y") then
                           perform 30-output-valid-record

                       else
                           perform 40-output-invalid-record
                            
                   end-if
                   else
                       perform 40-output-invalid-record
               end-if
               else 
                   perform 40-output-invalid-record
                    
               end-if
           else 
               perform 40-output-invalid-record
               
           end-if
        else 
            perform 40-output-invalid-record
             
           end-if

      * read next input-file record
           read input-file at end move "Y" to ws-eof-flag
           end-perform. 

       30-output-valid-record.
           add 1 to ws-valid.
           write valid-line from input-line.

       40-output-invalid-record.
           add 1 to ws-invalid.
           write invalid-line from input-line.
           write report-line from input-line.
           write report-line from ws-error-message.
           write report-line from ws-line-break.
       
       45-calculate-totals.
           add ws-valid ws-invalid giving ws-total-output.

       50-output-totals.
           write invalid-line from ws-line-break.
           write invalid-line from ws-line-break.
           write report-line from ws-total-valid.
           write invalid-line from ws-line-break.
           write report-line from ws-total-invalid.
           write report-line from ws-total.
       end program EDITS.