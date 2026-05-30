      *----------------------------------------------------------------*
      *  I#BVVEMD - MANUTENCAO DE DOMICILIO BANCARIO                   *
      *  ARQUIVO ASCG012                                               *
      *  REQUISICAO DE ALTERACAO DEMANUTENCAO DE DOMICILIO BANCARIO    *
      *----------------------------------------------------------------*

       01  BVVEMD-ASCG012.
           03 BVVEMD-GRUPO-ALTE-DOMCL  PIC X(006).
           03 BVVEMD-NUM-CTRL-IF       PIC X(020).
           03 BVVEMD-NUM-COD-IF        PIC X(003).
           03 BVVEMD-NUM-IDENT-SCG     PIC 9(017).
           03 BVVEMD-AG-CLI            PIC 9(004).
           03 BVVEMD-CT-CLI            PIC 9(013).
           03 BVVEMD-SINAL             PIC X(001).
           03 BVVEMD-PERC-GAR          PIC 9(005).
           03 BVVEMD-DTINI-VALIDD      PIC 9(008).
           03 BVVEMD-DTFIM-VALIDD      PIC 9(008).
           03 BVVEMD-TP-NATU-GAR       PIC X(001).
SVK        03 BVVEMD-SINAL-2           PIC X(001).
           03 BVVEMD-VLR-MAX-RET       PIC X(019).
