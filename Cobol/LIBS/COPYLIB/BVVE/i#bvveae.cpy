      *---------------------------------------------------------------*
      *     CADASTRO HISTORICO DE CREDITOS/ANTECIPACOES DO VENDEDOR   *
      *     CONTEM UM UNICO REGISTRO POR AGENCIA E CONTA OU NENHUM    *
      *     REGISTRO POR AGENCIA/CONTA                                *
      *     LRECL= 146          INC= I#BVVEAE          ENTRADA        *
      *---------------------------------------------------------------*

       01  REG-ARQRES00.
           02  RES00-AGENCIA         PIC 9(05)      COMP-3.
           02  RES00-CONTA           PIC 9(07)      COMP-3.
           02  RES00-PRODUTO         PIC 9(05)      COMP-3.
           02  RES00-ANTECIP-MES01   PIC S9(15)V99  COMP-3.
           02  RES00-ANTECIP-MES02   PIC S9(15)V99  COMP-3.
           02  RES00-ANTECIP-MES03   PIC S9(15)V99  COMP-3.
           02  RES00-ANTECIP-ATU     PIC S9(15)V99  COMP-3.
           02  RES00-ANTECIP-MES04   PIC S9(15)V99  COMP-3.
           02  RES00-CREDITO-ATU     PIC S9(15)V99  COMP-3.
           02  RES00-CREDITO-MES01   PIC S9(15)V99  COMP-3.
           02  RES00-CREDITO-MES02   PIC S9(15)V99  COMP-3.
           02  RES00-CREDITO-MES03   PIC S9(15)V99  COMP-3.
           02  RES00-DT-MOVTO        PIC X(10).
      *       FORMATO DD.MM.AAAA                                      *
           02  RES00-ANTECIP-MES06   PIC S9(15)V99  COMP-3.
           02  RES00-ANTECIP-MES05   PIC S9(15)V99  COMP-3.
           02  RES00-CREDITO-MES06   PIC S9(15)V99  COMP-3.
           02  RES00-CREDITO-MES04   PIC S9(15)V99  COMP-3.
           02  RES00-CREDITO-MES05   PIC S9(15)V99  COMP-3.
