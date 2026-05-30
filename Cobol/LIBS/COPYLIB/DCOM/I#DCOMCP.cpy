      *---------------------------------------------------------------*
      *                        I#DCOMCP                               *
      *    CADACEPN - CODIGO DE ENDERECAMENTO POSTAL - ROTINA DCOM    *
      *                      LRECL = 200                              *
      *---------------------------------------------------------------*

       01  CEPN-REG.
           05  CEPN-CEP.
               10  CEPN-CEP-NUMERO         PIC 9(05) COMP-3.
               10  CEPN-CEP-SUFIXO         PIC X(03).
           05  CEPN-CEP-ANTERIOR           PIC 9(05) COMP-3.
           05  CEPN-LOGRADOURO             PIC X(60).
           05  CEPN-CIDADE                 PIC X(30).
           05  CEPN-UF                     PIC X(02).
           05  CEPN-ENTR                   PIC 9(01).
           05  CEPN-NUMERACAO.
               10  CEPN-NUM-DE             PIC 9(05) COMP-3.
               10  CEPN-NUM-ATE            PIC 9(05) COMP-3.
           05  CEPN-LADO                   PIC X(01).
           05  CEPN-CDD.
               10  CEPN-CDD-IDENTIFICADOR  PIC 9(05) COMP-3.
               10  CEPN-NUM-NOME           PIC X(32).
           05  CEPN-DEPOSITARIA.
               10  CEPN-DEPOS-CODIGO       PIC 9(07) COMP-3.
               10  CEPN-DEPOS-NOME         PIC X(20).
           05  CEPN-MUNICIPIO.
               10  CEPN-MUNIC-CODIGO       PIC 9(05) COMP-3.
               10  CEPN-MUNIC-DAC          PIC X(01).
           05  FILLER                      PIC X(28).
