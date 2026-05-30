      *----------------------------------------------------------------*
      *  I#BVVEDL - MANUTENCAO DE DOMICILIO BANCARIO                   *
      *  CODIGO MENSAGEM CIP : SCG0016                                 *
      *  INFORMA SITUACAO DE ANTECIPACAO AO CLIENTE                    *
      *  FORMATO DATA/HORA: AAAAMMDDHHMMSSMMMMMM                       *
      *  FORMATO DATA: AAAAMMDD
      *----------------------------------------------------------------*

       01  BVVEDL-SCG0016.
           03 BVVEDL-COD-MSG           PIC X(009).
           03 BVVEDL-NUM-CTRL-CIP      PIC X(020).
           03 BVVEDL-ISPB-IF           PIC X(008).
           03 BVVEDL-NUM-IDENTC-SCG    PIC 9(017).
           03 BVVEDL-NOM-CREDDR        PIC X(080).
           03 BVVEDL-INDR-ANTEC        PIC X(001).
           03 BVVEDL-QTD-CREDDR        PIC 9(009).
           03 BVVEDL-QTD-RET           PIC 9(009).
           03 BVVEDL-DT-HR-SCG         PIC X(020).
           03 BVVEDL-DT-MOVTO          PIC 9(008).

