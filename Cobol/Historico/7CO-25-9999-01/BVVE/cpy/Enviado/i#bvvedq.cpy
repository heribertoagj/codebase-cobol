      *----------------------------------------------------------------*
      *  I#BVVEDQ - MANUTENCAO DE DOMICILIO BANCARIO - RETORNO         *
      *  CODIGO MENSAGEM CIP : SCG0011E                                *
      *  REQUISICAO DE INCLUSAO DE MANUTENCAO DE DOMICILIO BANCARIO    *
      *  FORMATO DATAS: AAAAMMDD                                       *
      *----------------------------------------------------------------*

       01  BVVEDQ-SCG0011E.
           03 BVVEDQ-COD-MSG           PIC X(009).
           03 BVVEDQ-COD-MSG-E         PIC X(008).
           03 BVVEDQ-NUM-CTRL-IF       PIC X(020).
           03 BVVEDQ-NUM-CTRL-IF-E     PIC X(008).
           03 BVVEDQ-ISPB-IF           PIC X(008).
           03 BVVEDQ-ISPB-IF-E         PIC X(008).
           03 BVVEDQ-DADOS-CLI.
              05 BVVEDQ-TP-PESSOA-CLI     PIC X(001).
              05 BVVEDQ-TP-PESSOA-CLI-E   PIC X(008).
7C2511        05 BVVEDQ-CNPJ-CPF-CLI      PIC X(014).
      *       05 BVVEDQ-CNPJ-CPF-CLI      PIC 9(014).
              05 BVVEDQ-CNPJ-CPF-CLI-E    PIC X(008).
           03 BVVEDQ-CNPJ-BASE-CLI     PIC X(008).
           03 BVVEDQ-CNPJ-BASE-CLI-E   PIC X(008).
           03 BVVEDQ-COD-PROPT-ESQM    PIC X(003).
           03 BVVEDQ-COD-PROPT-ESQM-E  PIC X(008).
           03 BVVEDQ-TP-PRODT          PIC X(001).
           03 BVVEDQ-TP-PRODT-E        PIC X(008).
           03 BVVEDQ-NUM-COD-IF        PIC X(003).
           03 BVVEDQ-NUM-COD-IF-E      PIC X(008).
           03 BVVEDQ-AG-CLI            PIC 9(004).
           03 BVVEDQ-AG-CLI-E          PIC X(008).
           03 BVVEDQ-CT-CLI            PIC 9(013).
           03 BVVEDQ-CT-CLI-E          PIC X(008).
           03 BVVEDQ-PERC-GAR          PIC 9(005).
           03 BVVEDQ-PERC-GAR-E        PIC X(008).
           03 BVVEDQ-DTINI-VALIDD      PIC 9(008).
           03 BVVEDQ-DTINI-VALIDD-E    PIC X(008).
           03 BVVEDQ-DTFIM-VALIDD      PIC 9(008).
           03 BVVEDQ-DTFIM-VALIDD-E    PIC X(008).
           03 BVVEDQ-TP-NATU-GAR       PIC X(001).
           03 BVVEDQ-TP-NATU-GAR-E     PIC X(008).
           03 BVVEDQ-DT-MOVTO          PIC 9(008).
           03 BVVEDQ-DT-MOVTO-E        PIC X(008).
