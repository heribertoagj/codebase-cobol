      *----------------------------------------------------------------*
      *  I#BVVEA4 - LIQUIDACAO FINANCEIRA                              *
      *  ARQUIVO ASCG022 E ASCG024                                     *
      *  FORMATO DATAS: AAAAMMDD                                       *
      *  FORMATO DATA/HORA: AAAA-MM-DDTHH:MM:SS:MMMMMM                 *
      *----------------------------------------------------------------*

       01  REG-ASCG022-ASCG024.
           03  BVVEA4-ID-REG                PIC X(01).

           03  BVVEA4-HEADER.
               05  BVVEA4-NOME-ARQ          PIC X(050).
               05  BVVEA4-NUM-CTRL-EMIS     PIC X(020).
               05  BVVEA4-NUM-CTRL-DEST-OR  PIC X(020).
               05  BVVEA4-ISPB-EMISSOR      PIC X(008).
               05  BVVEA4-ISPB-DESTINATARIO PIC X(008).
               05  BVVEA4-DT-HR-ARQ         PIC X(019).
               05  BVVEA4-SIT-CONS          PIC X(002).
               05  BVVEA4-GRUPO-SEQ.
                   07  BVVEA4-NUM-SEQ       PIC 9(009).
                   07  BVVEA4-INDR-CONT     PIC X(001).
               05  BVVEA4-DT-REF            PIC X(008).
               05  BVVEA4-TOTAL-GRAV        PIC 9(012).
               05  FILLER                   PIC X(232).

           03  BVVEA4-DETALHE REDEFINES BVVEA4-HEADER.
               05 BVVEA4-AGENDA.
                  07 BVVEA4-CNPJ-BASE-CREDC-X.
                    09 BVVEA4-CNPJ-BASE-CREDC    PIC 9(008).
                  07 BVVEA4-CNPJ-CREDC-X.
                    09 BVVEA4-CNPJ-CRE           PIC 9(014).
                  07 BVVEA4-ISPB-CREDORA         PIC X(008).
                  07 BVVEA4-AG-CREDC-X.
                    09 BVVEA4-AG-CREDC           PIC 9(004).
                  07 BVVEA4-CT-CREDC-X.
                    09 BVVEA4-CT-CREDC           PIC 9(013).
                  07 BVVEA4-NOME-CREDC           PIC X(080).
                  07 BVVEA4-GRP-CENTRALIZ.
                    09 BVVEA4-TP-PSSOA-CEN       PIC X(001).
                    09 BVVEA4-CNPJ-CPF-CEN-X.
                      11 BVVEA4-CNPJ-CPF-CEN     PIC 9(014).
                    09 BVVEA4-COD-CENTRALZ       PIC X(025).
                    09 BVVEA4-AG-CENTRALZ-X.
                      11 BVVEA4-AG-CENTRALZ      PIC 9(004).
                    09 BVVEA4-CT-CENTRALZ-X.
                      11 BVVEA4-CT-CENTRALZ      PIC 9(013).
                    09 BVVEA4-GRP-PONTO-VDA.
                       11 BVVEA4-NUM-LIQ         PIC X(021).
                       11 BVVEA4-ISPB-IF-LIQ     PIC X(008).
                       11 BVVEA4-CPONTO-VDA      PIC X(025).
                       11 BVVEA4-NOME-PTO-VEN    PIC X(080).
                       11 BVVEA4-TP-PSSOA-PVDA   PIC X(001).
                       11 BVVEA4-CNPJ-CPF-PVDA-X.
                        13 BVVEA4-CNPJ-CPF-PVDA  PIC 9(014).
                       11 BVVEA4-COD-ARRAJ-PGTO  PIC X(003).
                       11 BVVEA4-TP-PRODT-X.
                        13 BVVEA4-TP-PRODT       PIC 9(002).
                       11 BVVEA4-IDTF-TRANSF-X.
                        13 BVVEA4-IDTF-TRANSF    PIC 9(001).
                       11 BVVEA4-CMOEDA-X.
                        13 BVVEA4-CMOEDA         PIC 9(003).
                       11 BVVEA4-DT-PGTO-X.
                         13 BVVEA4-DT-PGTO       PIC 9(008).
                       11 BVVEA4-VLR-LIQD-PGTO   PIC X(020).
                       11 BVVEA4-DT-HORA         PIC X(019).

      *    DATA FORMATO AAAAMMDD
