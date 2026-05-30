      ***===========================================================***
      *** NOME INC                                     LENGTH=00320 ***
      *** I#SLIG15                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG          ***
      ***            ARQUIVO SUMARIZADO POR CLIENTE PARA EMISSAO    ***
      ***            DE RELATORIOS                                  ***
      ***                                                           ***
      ***===========================================================***
      *
       01  REG-SUMAR-TIT.
           03  SLIG15-CLIENTE-CARTEIRA.
               05  SLIG15-CGC-CPF-CLIENTE        PIC  9(009)    COMP-3.
               05  SLIG15-TP-PESS                PIC  X(001).
               05  SLIG15-TIPO-CART              PIC  9(001).
               05  SLIG15-NOME-CLIENTE           PIC  X(040).
            03 SLIG15-PERIODO.
               05  SLIG15-PER-INI                PIC  9(009)    COMP-3.
               05  SLIG15-PER-FIM                PIC  9(009)    COMP-3.
            03 SLIG15-TIT-MOVTO.
               05  SLIG15-TIT-VL-TOT-CART        PIC S9(013)V99 COMP-3.
               05  SLIG15-TIT-VL-VCTO-CART       PIC S9(013)V99 COMP-3.
               05  SLIG15-TIT-VL-VCTO-ANT        PIC S9(013)V99 COMP-3.
               05  SLIG15-TIT-VL-PGTO-CART       PIC S9(013)V99 COMP-3.
               05  SLIG15-TIT-VL-BAIXA-ANT       PIC S9(013)V99 COMP-3.
               05  SLIG15-TIT-VL-PRORROGA        PIC S9(013)V99 COMP-3.
               05  SLIG15-TIT-VL-ABATIMEN        PIC S9(013)V99 COMP-3.
               05  SLIG15-TIT-VL-PGTO-ANT        PIC S9(013)V99 COMP-3.
               05  SLIG15-TIT-VL-BX-SEM-PGTO     PIC S9(013)V99 COMP-3.
           03  SLIG15-CHQ-MOVTO.
               05  SLIG15-CHQ-VL-TOT-CART        PIC S9(013)V99 COMP-3.
               05  SLIG15-CHQ-VL-TOT-VCTO-CART   PIC S9(013)V99 COMP-3.
               05  SLIG15-CHQ-VL-VCTO-PER-PG     PIC S9(013)V99 COMP-3.
               05  SLIG15-CHQ-VL-VCTO-PER-NAO-PG PIC S9(013)V99 COMP-3.
               05  SLIG15-CHQ-VL-VCTO-PER-BXADO  PIC S9(013)V99 COMP-3.
               05  SLIG15-CHQ-VL-APOS-PER-PG     PIC S9(013)V99 COMP-3.
               05  SLIG15-CHQ-VL-APOS-PER-BXADO  PIC S9(013)V99 COMP-3.
               05  SLIG15-CHQ-VL-CHEQ-PRIM-DEVLC PIC S9(013)V99 COMP-3.
               05  SLIG15-CHQ-VL-CHEQ-REAP       PIC S9(013)V99 COMP-3.
               05  SLIG15-CHQ-VL-CHEQ-REAP-DEVOL PIC S9(013)V99 COMP-3.
               05  SLIG15-CHQ-VL-CHEQ-REAP-PG    PIC S9(013)V99 COMP-3.
               05  SLIG15-CHQ-VL-DEVOL-ANTER     PIC S9(013)V99 COMP-3.
           03  SLIG15-VND-MOVTO.
               05  SLIG15-VND-VL-TOT-CART        PIC S9(013)V99 COMP-3.
               05  SLIG15-VND-VL-VCTO-CART       PIC S9(013)V99 COMP-3.
               05  SLIG15-VND-VL-VCTO-ANT        PIC S9(013)V99 COMP-3.
               05  SLIG15-VND-VL-PGTO-CART       PIC S9(013)V99 COMP-3.
               05  SLIG15-VND-VL-BAIXA-ANT       PIC S9(013)V99 COMP-3.
               05  SLIG15-VND-VL-PGTO-ANT        PIC S9(013)V99 COMP-3.
               05  SLIG15-VND-VL-BX-SEM-PGTO     PIC S9(013)V99 COMP-3.
               05  SLIG15-VND-VL-PGTO-SACADO     PIC S9(013)V99 COMP-3.
               05  SLIG15-VND-VL-PGTO-FORNEC     PIC S9(013)V99 COMP-3.
               05  SLIG15-VND-VL-PGTO-ANT-SACADO PIC S9(013)V99 COMP-3.
               05  SLIG15-VND-VL-PGTO-ANT-FORNEC PIC S9(013)V99 COMP-3.

      * === CAMPOS DUPLICADOS PARA STRING ===
           03  SLIG15-CGC-CPF-CLIENTE-ST         PIC X(009).
           03  FILLER                            PIC  X(07).
