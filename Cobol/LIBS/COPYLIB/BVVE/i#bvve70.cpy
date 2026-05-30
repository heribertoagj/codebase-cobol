      *----------------------------------------------------------------*
      *  I#BVVE70 - LIQUIDACAO FINANCEIRA                              *
      *  FORMATO DATAS: AAAAMMDD                                       *
      *  FORMATO DATA/HORA: AAAA-MM-DDTHH:MM:SS:MMMMMM                 *
      *  TAMANHO LAYOUT: 550                                           *
      *----------------------------------------------------------------*

       01  BVVE70-REGISTRO.
           03  BVVE70-ID-REG                     PIC  X(001).
           03  BVVE70-HEADER.
               05  BVVE70-NOME-ARQ               PIC  X(050).
               05  BVVE70-NUM-CTRL-EMIS          PIC  X(020).
               05  BVVE70-NUM-CTRL-DEST-OR       PIC  X(020).
               05  BVVE70-ISPB-EMISSOR           PIC  X(008).
               05  BVVE70-ISPB-DESTINATARIO      PIC  X(008).
               05  BVVE70-DT-HR-ARQ              PIC  X(026).
               05  BVVE70-SIT-CONS               PIC  X(002).
               05  FILLER                        PIC  X(006).
               05  BVVE70-DT-REF                 PIC  X(008).
               05  FILLER                        PIC  X(408).
               05  BVVE70-CSEQ-REG               PIC S9(015) COMP-3.
               05  FILLER                        PIC  X(035).

           03  BVVE70-DETALHE REDEFINES BVVE70-HEADER.
               05 BVVE70-GRUPO-LIQUID.
                  07 BVVE70-IDENTD-PART-PRINC    PIC  X(008).
                  07 BVVE70-IDENTD-PART-ADM      PIC  X(008).
                  07 BVVE70-CNPJ-BASE-CREDC-X.
                    09 BVVE70-CNPJ-BASE-CREDC    PIC  9(008).
                  07 BVVE70-CNPJ-CREDC-X.
                    09 BVVE70-CNPJ-CRE           PIC  9(014).
                  07 BVVE70-ISPB-DEVEDOR         PIC  X(008).
                  07 BVVE70-ISPB-CREDORA         PIC  X(008).
                  07 BVVE70-AG-CREDC-X.
                    09 BVVE70-AG-CREDC           PIC  9(004).
                  07 BVVE70-CT-CREDC-X.
                    09 BVVE70-CT-CREDC           PIC  9(013).
                  07 BVVE70-NOME-CREDC           PIC  X(080).
                  07 BVVE70-GRP-CENTRALIZ.
                    09 BVVE70-TP-PSSOA-CEN       PIC  X(001).
                    09 BVVE70-CNPJ-CPF-CEN-X.
                      11 BVVE70-CNPJ-CPF-CEN     PIC  9(014).
                    09 BVVE70-COD-CENTRALZ       PIC  X(025).
                    09 BVVE70-TP-CONTA           PIC  X(002).
                    09 BVVE70-AG-CENTRALZ-X.
                      11 BVVE70-AG-CENTRALZ      PIC  9(004).
                    09 BVVE70-CT-CENTRALZ-X.
                      11 BVVE70-CT-CENTRALZ      PIC  9(013).
                    09 BVVE70-CT-PGTO-CENTRLZ    PIC  X(020).
                    09 BVVE70-GRP-PONTO-VDA.
                       11 BVVE70-NUM-LIQ         PIC  X(021).
                       11 BVVE70-ISPB-IF-LIQ     PIC  X(008).
                       11 BVVE70-CPONTO-VDA      PIC  X(025).
                       11 BVVE70-NOME-PTO-VEN    PIC  X(080).
                       11 BVVE70-TP-PSSOA-PVDA   PIC  X(001).
                       11 BVVE70-CNPJ-CPF-PVDA-X.
                        13 BVVE70-CNPJ-CPF-PVDA  PIC  9(014).
                       11 BVVE70-COD-ARRAJ-PGTO  PIC  X(003).
                       11 BVVE70-TP-PRODT-X.
                        13 BVVE70-TP-PRODT       PIC  9(002).
                       11 BVVE70-IDTF-TRANSF-X.
                        13 BVVE70-IDTF-TRANSF    PIC  9(001).
                       11 BVVE70-CMOEDA-X.
                        13 BVVE70-CMOEDA         PIC  9(003).
                       11 BVVE70-DT-PGTO-X.
                         13 BVVE70-DT-PGTO       PIC  9(008).
                       11 BVVE70-VLR-LIQD-PGTO   PIC    X(020).
                       11 BVVE70-DT-HORA         PIC  X(019).
                       11 BVVE70-TP-PTO-VDA      PIC  X(002).
                       11 BVVE70-TP-VLR-PGTO     PIC  X(002).
               05  FILLER                        PIC  X(110).
               05  BVVE70-CCRDRA-ESTBL-COML      PIC  9(004).
               05  BVVE70-CBANDE-VISAO           PIC  9(003).
               05  BVVE70-CSEQ-REG-DET           PIC S9(015) COMP-3.
               05  FILLER                        PIC  X(035).
