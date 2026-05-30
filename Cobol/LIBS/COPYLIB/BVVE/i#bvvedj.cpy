      *----------------------------------------------------------------*
      *  I#BVVEDJ - MANUTENCAO DE DOMICILIO BANCARIO                   *
      *  CODIGO MENSAGEM CIP : GEN0004                                 *
      *  INFORMA ERRO DE TRANSMISSAO                                   *
      *  FORMATO DATA/HORA: AAAAMMDDHHMMSSMMMMMM                       *
      *----------------------------------------------------------------*

       01  BVVEDJ-GEN0004.
           03 BVVEDJ-COD-MSG           PIC X(009).
           03 BVVEDJ-ERRO-GEN          PIC X(008).
           03 BVVEDJ-ISPB-EMISSOR      PIC X(008).
           03 BVVEDJ-ISPB-DESTIN       PIC X(008).
           03 BVVEDJ-NUM-MQ            PIC X(048).
           03 BVVEDJ-NU-OP-OR          PIC X(023).
           03 BVVEDJ-NUM-PROT-PSTA-OR  PIC X(008).
           03 BVVEDJ-HIST              PIC X(200).
           03 BVVEDJ-DT-HR-PART        PIC X(020).

