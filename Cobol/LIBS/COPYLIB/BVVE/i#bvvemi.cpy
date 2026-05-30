      *----------------------------------------------------------------*
      *  I#BVVEMI - MANUTENCAO DE DOMICILIO BANCARIO - EM MASSA        *
      *  SOLICITACOES DE EXCLUSAO  COM RETORNO                         *
      *  FORMATO DATAS: AAAAMMDD                                       *
      *----------------------------------------------------------------*

       01  BVVEMI-REGISTRO.
           03  BVVEMI-COD-MSG          PIC X(007).
           03  BVVEMI-ORIGEM           PIC X(001).
           03  BVVEMI-NUMCTRL-IF-ENVIO PIC X(020).
           03  BVVEMI-DADOS-CLI.
               05 BVVEMI-TP-PESSOA-CLI PIC X(001).
               05 BVVEMI-CNPJ-CLI      PIC 9(014).
               05 BVVEMI-CPF-CLI  REDEFINES BVVEMI-CNPJ-CLI.
                  07 FILLER            PIC 9(003).
                  07 BVVEMI-CPF-N      PIC 9(011).
           03  BVVEMI-TRAVA-RAIZ       PIC X(001).
           03  BVVEMI-NUMSCG           PIC 9(017).
           03  BVVEMI-TP-BAIXA         PIC X(001).
           03  BVVEMI-DT-MOVTO         PIC 9(008).
           03  BVVEMI-BANDEIRA         PIC 9(003).
           03  BVVEMI-COD-PROP-ESQ     PIC X(003).
           03  BVVEMI-AREA-REJEITADOS.
               05  BVVEMI-ERRMSG       PIC X(008) OCCURS 10.
