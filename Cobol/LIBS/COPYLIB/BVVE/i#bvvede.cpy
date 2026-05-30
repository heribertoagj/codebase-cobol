      *----------------------------------------------------------------*
      *  I#BVVEDE - MANUTENCAO DE DOMICILIO BANCARIO                   *
      *  CODIGO MENSAGEM CIP : SCG0012                                 *
      *  REQUISICAO DE ALTERACAO DE MANUTENCAO DE DOMICILIO BANCARIO   *
      *  FORMATO DATAS: AAAAMMDD                                       *
      *----------------------------------------------------------------*

       01  BVVEDE-SCG0012.
           03 BVVEDE-COD-MSG           PIC X(009).
           03 BVVEDE-NUM-CTRL-IF       PIC X(020).
           03 BVVEDE-ISPB-IF           PIC X(008).
           03 BVVEDE-NUM-COD-IF        PIC X(003).
           03 BVVEDE-NUM-IDENTC-SCG    PIC 9(017).
           03 BVVEDE-AG-CLI            PIC 9(004).
           03 BVVEDE-CT-CLI            PIC 9(013).
           03 BVVEDE-PERC-GAR          PIC 9(005).
           03 BVVEDE-DTINI-VALIDD      PIC 9(008).
           03 BVVEDE-DTFIM-VALIDD      PIC 9(008).
           03 BVVEDE-TP-NATU-GAR       PIC X(001).
           03 BVVEDE-VALOR             PIC X(019).
           03 BVVEDE-DT-MOVTO          PIC 9(008).
