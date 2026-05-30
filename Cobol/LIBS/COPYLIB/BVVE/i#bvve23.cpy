      ***********************************************************
      *                                                         *
      *    INC  'I#BVVE23'                  LRECL = 2048        *
      *                                                         *
      *    AREA    DE COMUNICACAO ENTRE MAINFRAME E ISD         *
      *                  RETORNO DO MF(BVVE7910)  PARA ISD      *
      *    MODULO      - MODULO QUE ESTA SENDO CHAMADO          *
      *    COD RETORNO - IDENTIFICA SE HOUVE ERRO NO PROCESSO   *
      *                  0 - OK                                 *
      *                  1 - ERRO CONSISTENCIA                  *
      *                  2 - ERRO DE LOGICA - MOSTRA MSG HOST   *
      *                  3 - ERRO GRAVA SQL - MOSTRA MSG SQL    *
      *                                                         *
      ***********************************************************

       01  BVVE23-COMUNIC-ROT-PLATBX.
           03  BVVE23-SAI-LL                    PIC  9(04) COMP.
           03  BVVE23-SAI-ZZ                    PIC  9(04) COMP.
           03  BVVE23-COD-RETORNO               PIC  9(002).
           03  BVVE23-SQLCODE                   PIC S9(005).
           03  BVVE23-TAMANHO                   PIC  9(004).
           03  BVVE23-AREA-DADOS                PIC  X(2033).
