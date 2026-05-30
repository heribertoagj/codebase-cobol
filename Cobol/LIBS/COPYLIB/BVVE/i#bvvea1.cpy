      *----------------------------------------------------------------*
      *  I#BVVEA1 - MANUTENCAO DE DOMICILIO BANCARIO                   *
      *  ARQUIVO ASCG021                                               *
      *  AGENDA FINANCEIRA GETNET ELAVON                               *
      *  FORMATO DATAS: AAAAMMDD                                       *
      *  FORMATO DATA/HORA: AAAA-MM-DDTHH:MM:SS:MMMMMM                 *
      *----------------------------------------------------------------*

       01  REG-ASCG021.
           03  BVVEA1-ID-REG                PIC X(01).

           03  BVVEA1-HEADER.
               05  BVVEA1-NOME-ARQ          PIC X(050).
               05  BVVEA1-NUM-CTRL-EMIS     PIC X(020).
               05  BVVEA1-NUM-CTRL-DEST-OR  PIC X(020).
               05  BVVEA1-ISPB-EMISSOR      PIC X(008).
               05  BVVEA1-ISPB-DESTINATARIO PIC X(008).
               05  BVVEA1-DT-HR-ARQ         PIC X(019).
               05  BVVEA1-SIT-CONS          PIC X(002).
               05  BVVEA1-GRUPO-SEQ.
                   07  BVVEA1-NUM-SEQ       PIC 9(009).
                   07  BVVEA1-INDR-CONT     PIC X(001).
               05  BVVEA1-DT-REF            PIC X(008).
               05  BVVEA1-TOTAL-GRAV        PIC 9(012).
               05  FILLER                   PIC X(102).

           03  BVVEA1-DETALHE REDEFINES BVVEA1-HEADER.
               05 BVVEA1-AGENDA.
                  07 BVVEA1-CNPJ-BASE-CREDC-X.
                    09 BVVEA1-CNPJ-BASE-CREDC    PIC 9(008).
                  07 BVVEA1-CNPJ-CREDC-X.
                    09 BVVEA1-CNPJ-CRE           PIC 9(014).
                  07 BVVEA1-ISPB-CREDORA         PIC X(008).
                  07 BVVEA1-AG-CREDC-X.
                    09 BVVEA1-AG-CREDC           PIC 9(004).
                  07 BVVEA1-CT-CREDC-X.
                    09 BVVEA1-CT-CREDC           PIC 9(013).
                  07 BVVEA1-NOME-CREDC           PIC X(080).
                  07 BVVEA1-GRP-CENTRALIZ.
                    09 BVVEA1-TP-PSSOA-CEN       PIC X(001).
                    09 BVVEA1-CNPJ-CPF-CEN-X.
                      11 BVVEA1-CNPJ-CPF-CEN     PIC 9(014).
                    09 BVVEA1-COD-CENTRALZ       PIC X(025).
                    09 BVVEA1-AG-CENTRALZ-X.
                      11 BVVEA1-AG-CENTRALZ      PIC 9(004).
                    09 BVVEA1-CT-CENTRALZ-X.
                      11 BVVEA1-CT-CENTRALZ      PIC 9(013).
                    09 BVVEA1-GRP-PONTO-VDA.
                       11 BVVEA1-COD-ARRAJ-PGTO  PIC X(003).
                       11 BVVEA1-CMOEDA-X.
                        13 BVVEA1-CMOEDA         PIC 9(003).
                       11 BVVEA1-CPONTO-VDA      PIC X(025).
                       11 BVVEA1-TP-PSSOA-PVDA   PIC X(001).
                       11 BVVEA1-CNPJ-CPF-PVDA-X.
                        13 BVVEA1-CNPJ-CPF-PVDA  PIC 9(014).
                       11 BVVEA1-DT-PGTO-X.
                         13 BVVEA1-DT-PGTO       PIC 9(008).
                       11 BVVEA1-VLR-LIQD-PGTO   PIC X(020).

      *    DATA FORMATO AAAAMMDD
