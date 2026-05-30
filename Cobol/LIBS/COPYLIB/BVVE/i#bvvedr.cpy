      *----------------------------------------------------------------*
      *  I#BVVEDE - MANUTENCAO DE DOMICILIO BANCARIO - RETORNO
      *  CODIGO MENSAGEM CIP : SCG0012E                                *
      *  REQUISICAO DE ALTERACAO DE MANUTENCAO DE DOMICILIO BANCARIO   *
      *  FORMATO DATAS: AAAAMMDD                                       *
      *----------------------------------------------------------------*

       01  BVVEDR-SCG0012E.
           03 BVVEDR-COD-MSG           PIC X(009).
           03 BVVEDR-COD-MSG-E         PIC X(008).
           03 BVVEDR-NUM-CTRL-IF       PIC X(020).
           03 BVVEDR-NUM-CTRL-IF-E     PIC X(008).
           03 BVVEDR-ISPB-IF           PIC X(008).
           03 BVVEDR-ISPB-IF-E         PIC X(008).
           03 BVVEDR-NUM-COD-IF        PIC X(003).
           03 BVVEDR-NUM-COD-IF-E      PIC X(008).
           03 BVVEDR-NUM-IDENTC-SCG    PIC 9(017).
           03 BVVEDR-NUM-IDENTC-SCG-E  PIC X(008).
           03 BVVEDR-AG-CLI            PIC 9(004).
           03 BVVEDR-AG-CLI-E          PIC X(008).
           03 BVVEDR-CT-CLI            PIC 9(013).
           03 BVVEDR-CT-CLI-E          PIC X(008).
           03 BVVEDR-PERC-GAR          PIC 9(005).
           03 BVVEDR-PERC-GAR-E        PIC X(008).
           03 BVVEDR-DTINI-VALIDD      PIC 9(008).
           03 BVVEDR-DTINI-VALIDD-E    PIC X(008).
           03 BVVEDR-DTFIM-VALIDD      PIC 9(008).
           03 BVVEDR-DTFIM-VALIDD-E    PIC X(008).
           03 BVVEDR-TP-NATU-GAR       PIC X(001).
           03 BVVEDR-TP-NATU-GAR-E     PIC X(008).
           03 BVVEDR-DT-MOVTO          PIC 9(008).
           03 BVVEDR-DT-MOVTO-E        PIC X(008).
