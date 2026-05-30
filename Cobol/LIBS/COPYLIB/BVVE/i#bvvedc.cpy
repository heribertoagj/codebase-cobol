      *----------------------------------------------------------------*
      *  I#BVVEDC - MANUTENCAO DE DOMICILIO BANCARIO                   *
      *  CODIGO MENSAGEM CIP : SCG0011                                 *
      *  REQUISICAO DE INCLUSAO DE MANUTENCAO DE DOMICILIO BANCARIO    *
      *  FORMATO DATAS: AAAAMMDD                                       *
      *  TAMANHO: 134
      *----------------------------------------------------------------*

       01  BVVEDC-SCG0011.
           03 BVVEDC-COD-MSG           PIC X(009).
           03 BVVEDC-NUM-CTRL-IF       PIC X(020).
           03 BVVEDC-ISPB-IF           PIC X(008).
           03 BVVEDC-DADOS-CLI.
              05 BVVEDC-TP-PESSOA-CLI  PIC X(001).
              05 BVVEDC-CNPJ-CPF-CLI   PIC 9(014).
           03 BVVEDC-CNPJ-BASE-CLI     PIC X(008).
           03 BVVEDC-COD-PROPT-ESQM    PIC X(003).
           03 BVVEDC-TP-PRODT          PIC X(001).
           03 BVVEDC-NUM-COD-IF        PIC X(003).
           03 BVVEDC-AG-CLI            PIC 9(004).
           03 BVVEDC-CT-CLI            PIC 9(013).
           03 BVVEDC-PERC-GAR          PIC 9(005).
           03 BVVEDC-DTINI-VALIDD      PIC 9(008).
           03 BVVEDC-DTFIM-VALIDD      PIC 9(008).
           03 BVVEDC-TP-NATU-GAR       PIC X(001).
           03 BVVEDC-VALOR             PIC X(019).
           03 BVVEDC-DT-MOVTO          PIC 9(008).
