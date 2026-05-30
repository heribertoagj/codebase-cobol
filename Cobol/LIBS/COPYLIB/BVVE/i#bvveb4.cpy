      *===============================================================*
      *    FORMATACAO DO ARQUVIO PARA ROTINA CACF                     *
      *    BOOK = I#BVVEB4                TAMANHO = 100               *
      *===============================================================*
      *
       01  BVVEB4-ARQCACF.
           03  BVVEB4-HEADER.
               05  BVVEB4-HD-SPBIF-CREDR     PIC 9(008).
               05  BVVEB4-HD-SEQ             PIC 9(003).
               05  BVVEB4-HD-QT-REG          PIC 9(009).
               05  FILLER                    PIC X(022).
           03  BVVEB4-DETALHE REDEFINES
               BVVEB4-HEADER.
               05  BVVEB4-DT-CNPJ-CRED       PIC 9(008).
               05  BVVEB4-DT-COD-PROD        PIC X(003).
               05  BVVEB4-DT-DEB-CRED        PIC X(001).
               05  BVVEB4-DT-VLR-PGTO        PIC 9(015)V99.
               05  BVVEB4-DT-DATA-PGTO       PIC 9(008).
               05  BVVEB4-QTD-REG            PIC 9(005).

