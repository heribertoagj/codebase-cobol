      *----------------------------------------------------------------*
      *  I#BVVEB1 - LIQUIDACAO FINANCEIRA                              *
      *  ARQUIVOS ASLC022/024/032                                      *
      *  FORMATO DATAS: AAAAMMDD                                       *
      *  FORMATO DATA/HORA: AAAA-MM-DDTHH:MM:SS:MMMMMM                 *
      *----------------------------------------------------------------*
      *  24/10/2018 SIVEK   INCLUIDO CAMPOS TIPO PONTO DE VENDA E TIPO *
      *                     VALOR DE PAGTO                             *
      *----------------------------------------------------------------*
      *  06/11/2019 ISRAEL  INCLUIDO CAMPO FLAG PARA TIPO DE RETENCAO  *
      *             SIS     TOTAL = T   PARCIAL = P   NAO RETIDO = N   *
      *----------------------------------------------------------------*

       01  REG-ASLC.
           03  BVVEB1-ID-REG                PIC X(001).
           03  BVVEB1-HEADER.
               05  BVVEB1-NOME-ARQ          PIC X(050).
               05  BVVEB1-NUM-CTRL-EMIS     PIC X(020).
               05  BVVEB1-NUM-CTRL-DEST-OR  PIC X(020).
               05  BVVEB1-ISPB-EMISSOR      PIC X(008).
               05  BVVEB1-ISPB-DESTINATARIO PIC X(008).
               05  BVVEB1-DT-HR-ARQ         PIC X(026).
               05  BVVEB1-SIT-CONS          PIC X(002).
               05  FILLER                   PIC X(006).
               05  BVVEB1-DT-REF            PIC X(008).
SVK            05  FILLER                   PIC X(292).

           03  BVVEB1-DETALHE REDEFINES BVVEB1-HEADER.
               05 BVVEB1-GRUPO-LIQUID.
                  07 BVVEB1-IDENTD-PART-PRINC    PIC X(008).
                  07 BVVEB1-IDENTD-PART-ADM      PIC X(008).
                  07 BVVEB1-CNPJ-BASE-CREDC-X.
                    09 BVVEB1-CNPJ-BASE-CREDC    PIC 9(008).
                  07 BVVEB1-CNPJ-CREDC-X.
                    09 BVVEB1-CNPJ-CRE           PIC 9(014).
                  07 BVVEB1-ISPB-DEVEDOR         PIC X(008).
                  07 BVVEB1-ISPB-CREDORA         PIC X(008).
                  07 BVVEB1-AG-CREDC-X.
                    09 BVVEB1-AG-CREDC           PIC 9(004).
                  07 BVVEB1-CT-CREDC-X.
                    09 BVVEB1-CT-CREDC           PIC 9(013).
                  07 BVVEB1-NOME-CREDC           PIC X(080).
                  07 BVVEB1-GRP-CENTRALIZ.
                    09 BVVEB1-TP-PSSOA-CEN       PIC X(001).
                    09 BVVEB1-CNPJ-CPF-CEN-X.
                      11 BVVEB1-CNPJ-CPF-CEN     PIC 9(014).
                    09 BVVEB1-COD-CENTRALZ       PIC X(025).
                    09 BVVEB1-TP-CONTA           PIC X(002).
                    09 BVVEB1-AG-CENTRALZ-X.
                      11 BVVEB1-AG-CENTRALZ      PIC 9(004).
                    09 BVVEB1-CT-CENTRALZ-X.
                      11 BVVEB1-CT-CENTRALZ      PIC 9(013).
                    09 BVVEB1-CT-PGTO-CENTRLZ    PIC X(020).
                    09 BVVEB1-GRP-PONTO-VDA.
                       11 BVVEB1-NUM-LIQ         PIC X(021).
                       11 BVVEB1-ISPB-IF-LIQ     PIC X(008).
                       11 BVVEB1-CPONTO-VDA      PIC X(025).
                       11 BVVEB1-NOME-PTO-VEN    PIC X(080).
                       11 BVVEB1-TP-PSSOA-PVDA   PIC X(001).
                       11 BVVEB1-CNPJ-CPF-PVDA-X.
                        13 BVVEB1-CNPJ-CPF-PVDA  PIC 9(014).
                       11 BVVEB1-COD-ARRAJ-PGTO  PIC X(003).
                       11 BVVEB1-TP-PRODT-X.
                        13 BVVEB1-TP-PRODT       PIC 9(002).
                       11 BVVEB1-IDTF-TRANSF-X.
                        13 BVVEB1-IDTF-TRANSF    PIC 9(001).
                       11 BVVEB1-CMOEDA-X.
                        13 BVVEB1-CMOEDA         PIC 9(003).
                       11 BVVEB1-DT-PGTO-X.
                         13 BVVEB1-DT-PGTO       PIC 9(008).
                       11 BVVEB1-VLR-LIQD-PGTO   PIC X(020).
                       11 BVVEB1-DT-HORA         PIC X(019).
SVK                    11 BVVEB1-TP-PTO-VDA      PIC X(002).
SVK                    11 BVVEB1-TP-VLR-PGTO     PIC X(002).
IM1               05 BVVEB1-FLAG-RET             PIC X(001).

      *    DATA FORMATO AAAAMMDD
