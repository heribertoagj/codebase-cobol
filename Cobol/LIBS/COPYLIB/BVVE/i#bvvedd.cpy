      *----------------------------------------------------------------*
      *  I#BVVEDD - MANUTENCAO DE DOMICILIO BANCARIO                   *
      *  CODIGO MENSAGEM CIP : SCG0011R1                               *
      *  RESPOSTA REQUIS INCLUSAO DE MANUTENCAO DE DOMICILIO BANCARIO  *
      *  FORMATO DATAS: AAAAMMDD                                       *
      *  FORMATO DATA/HORA: AAAAMMDDHHMMSSMMMMMM                       *
      *----------------------------------------------------------------*

       01  BVVEDD-SCG0011R1.
           03 BVVEDD-COD-MSG           PIC X(009).
           03 BVVEDD-NUM-CTRL-IF       PIC X(020).
           03 BVVEDD-ISPB-IF           PIC X(008).
           03 BVVEDD-NUM-CTRL-CIP      PIC X(020).
           03 BVVEDD-NUM-IDENTC-SCG    PIC 9(017).
           03 BVVEDD-DT-HR-SCG         PIC X(020).
           03 BVVEDD-DT-MOVTO          PIC 9(008).
