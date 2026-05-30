      ***===========================================================***
CNPJ7C*** NOME INC                               LENGTH=(0021) 30   ***
      *** I#SLIGFC                                                  ***
      ***-----------------------------------------------------------***
      **                                                             **
      **  DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG           **
      **             ARQUIVO DE UNLOAD DA TABELA SLIGB045            **
      **                                                             **
      ***===========================================================***
      *DATA        PROJETO                                 ANALISTA    *
      *01/08/2025  CNPJ ALFANUMERICO                       LUCIANA SATO*
      *----------------------------------------------------------------*
       05  SLIGFC-REGISTRO.
           10 SLIGFC-CCGC-CPF-SACDO      PIC S9(09)      COMP-3.
           10 SLIGFC-CINDCD-PSSOA-SACDO  PIC  X(01).
           10 SLIGFC-PPRTCP-CART-SIMPE   PIC S9(03)V99   COMP-3.
           10 SLIGFC-PPRTCP-CART-SIMPE-N PIC  X(01).
           10 SLIGFC-PPRTCP-CART-CAUCN   PIC S9(03)V99   COMP-3.
           10 SLIGFC-PPRTCP-CART-CAUCN-N PIC  X(01).
           10 SLIGFC-PPRTCP-CART-DESC    PIC S9(03)V99   COMP-3.
           10 SLIGFC-PPRTCP-CART-DESC-N  PIC  X(01).
CNPJ7C     10 SLIGFC-CCGC-CPF-SACDO-N    PIC  X(09).
