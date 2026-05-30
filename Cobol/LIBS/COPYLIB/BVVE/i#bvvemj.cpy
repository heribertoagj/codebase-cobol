      *----------------------------------------------------------------*
      *  I#BVVEMJ    - MANUTENCAO DE DOMICILIO BANCARIO - EM MASSA     *
      *  SOLICITACOES DE INCLUSAO E ALTERACAO                          *
      *  FORMATO DATAS: AAAAMMDD                                       *
      *----------------------------------------------------------------*

       01  BVVEMJ-REGISTRO.
           03  BVVEMJ-COD-MSG          PIC X(007).
           03  BVVEMJ-ORIGEM           PIC X(001).
           03  BVVEMJ-NUMCTRL-IF-ENVIO PIC X(020).
           03  BVVEMJ-DADOS-CLI.
               05 BVVEMJ-TP-PESSOA-CLI PIC X(001).
               05 BVVEMJ-CNPJ-CLI      PIC 9(014).
               05 BVVEMJ-CPF-CLI  REDEFINES BVVEMJ-CNPJ-CLI.
                  07 FILLER            PIC 9(003).
                  07 BVVEMJ-CPF-N      PIC 9(011).
           03  BVVEMJ-TRAVA-RAIZ       PIC X(001).
           03  BVVEMJ-COD-PROPT-ESQM   PIC X(003).
           03  BVVEMJ-TP-PRODT         PIC X(001).
           03  BVVEMJ-NUM-COD-IF       PIC X(003).
           03  BVVEMJ-AG-CLI           PIC 9(004).
           03  BVVEMJ-CT-CLI           PIC 9(013).
           03  BVVEMJ-PERC-GAR         PIC 9(005).
           03  BVVEMJ-DTINI-VALIDD     PIC 9(008).
           03  BVVEMJ-DTFIM-VALIDD     PIC 9(008).
           03  BVVEMJ-TP-NATU-GAR      PIC X(001).
           03  BVVEMJ-DT-MOVTO         PIC 9(008).
           03  BVVEMJ-BANDEIRA         PIC 9(003).
           03  BVVEMJ-NUMSCG           PIC 9(017).
           03  BVVEMJ-VLR-MAX-RET      PIC 9(013)V99 COMP-3.
           03  BVVEMJ-AREA-REJEITADOS.
               05  BVVEMJ-ERRMSG       PIC X(008) OCCURS 14.
