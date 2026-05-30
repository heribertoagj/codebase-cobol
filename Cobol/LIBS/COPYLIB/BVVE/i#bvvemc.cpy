      *----------------------------------------------------------------*
      *  I#BVVEMC - MANUTENCAO DE DOMICILIO BANCARIO                   *
      *  ARQUIVO ASCG011                                               *
      *  REQUISICAO DE INCLUSAO DE MANUTENCAO DE DOMICILIO BANCARIO    *
      *----------------------------------------------------------------*

       01  BVVEMC-ASCG011.
           03 BVVEMC-GRUPO-INCL-DOMCL  PIC X(006).
           03 BVVEMC-NUM-CTRL-IF       PIC X(020).
           03 BVVEMC-DADOS-CLI.
              05 BVVEMC-TP-PESSOA-CLI  PIC X(001).
              05 BVVEMC-CNPJ-CPF-CLI   PIC 9(014).
              05 BVVEMC-CPF            REDEFINES
                 BVVEMC-CNPJ-CPF-CLI.
                 07 BVVEMC-ZEROS       PIC 9(003).
                 07 BVVEMC-CPF-NUM     PIC 9(011).
           03 BVVEMC-CNPJ-BASE-CLI     PIC X(008).
           03 BVVEMC-COD-PROPT-ESQM    PIC X(003).
           03 BVVEMC-TP-PRODT          PIC X(001).
           03 BVVEMC-NUM-COD-IF        PIC X(003).
           03 BVVEMC-AG-CLI            PIC 9(004).
           03 BVVEMC-CT-CLI            PIC 9(013).
           03 BVVEMC-SINAL             PIC X(001).
           03 BVVEMC-PERC-GAR          PIC 9(005).
           03 BVVEMC-DTINI-VALIDD      PIC 9(008).
           03 BVVEMC-DTFIM-VALIDD      PIC 9(008).
           03 BVVEMC-TP-NATU-GAR       PIC X(001).
SVK        03 BVVEMC-SINAL-2           PIC X(001).
           03 BVVEMC-VLR-MAX-RET       PIC X(019).
