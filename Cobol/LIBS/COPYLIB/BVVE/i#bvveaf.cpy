      *---------------------------------------------------------------*
      *     CADASTRO HISTORICO DE CREDITOS/ANTECIPACOES DO VENDEDOR   *
      *     ATUALIZADO                                                *
      *     LRECL= 146          INC= I#BVVEAF          SAIDA          *
      *---------------------------------------------------------------*

       01  REG-ARQRES01.
           02  RES01-AGENCIA         PIC 9(05)      COMP-3.
           02  RES01-CONTA           PIC 9(07)      COMP-3.
           02  RES01-PRODUTO         PIC 9(05)      COMP-3.
           02  RES01-ANTECIP-MES01   PIC S9(15)V99  COMP-3.
           02  RES01-ANTECIP-MES02   PIC S9(15)V99  COMP-3.
           02  RES01-ANTECIP-MES03   PIC S9(15)V99  COMP-3.
           02  RES01-ANTECIP-ATU     PIC S9(15)V99  COMP-3.
           02  RES01-ANTECIP-MES04   PIC S9(15)V99  COMP-3.
           02  RES01-CREDITO-ATU     PIC S9(15)V99  COMP-3.
           02  RES01-CREDITO-MES01   PIC S9(15)V99  COMP-3.
           02  RES01-CREDITO-MES02   PIC S9(15)V99  COMP-3.
           02  RES01-CREDITO-MES03   PIC S9(15)V99  COMP-3.
           02  RES01-DT-MOVTO        PIC X(10).
      *       FORMATO DD.MM.AAAA                                      *
           02  RES01-ANTECIP-MES06   PIC S9(15)V99  COMP-3.
           02  RES01-ANTECIP-MES05   PIC S9(15)V99  COMP-3.
           02  RES01-CREDITO-MES06   PIC S9(15)V99  COMP-3.
           02  RES01-CREDITO-MES04   PIC S9(15)V99  COMP-3.
           02  RES01-CREDITO-MES05   PIC S9(15)V99  COMP-3.
