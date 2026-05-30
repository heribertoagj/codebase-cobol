      *----------------------------------------------------------------*
      *  I#BVVEDF - MANUTENCAO DE DOMICILIO BANCARIO                   *
      *  CODIGO MENSAGEM CIP : SCG0012R1/SCG0013R1                     *
      *  RESPOSTA ALTERACAO/BAIXA DE MANUTENCAO DE DOMICILIO BANCARIO  *
      *  FORMATO DATAS: AAAAMMDD                                       *
      *  FORMATO DATA/HORA: AAAAMMDDHHMMSSMMMMMM                       *
      *----------------------------------------------------------------*

       01  BVVEDF-SCG0012R1.
           03 BVVEDF-COD-MSG           PIC X(009).
           03 BVVEDF-NUM-CTRL-IF       PIC X(020).
           03 BVVEDF-ISPB-IF           PIC X(008).
           03 BVVEDF-NUM-CTRL-CIP      PIC X(020).
           03 BVVEDF-DT-HR-SCG         PIC X(014).
           03 BVVEDF-DT-MOVTO          PIC 9(008).
