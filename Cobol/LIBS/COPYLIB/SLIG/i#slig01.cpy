      ***===========================================================***
      *** NOME INC                                     LENGTH=0150  ***
      *** I#SLIG01                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG          ***
      ***            ARQUIVO REDUZIDO DE T.TULOS PENDENTES/F6EBFINA ***
      ***                                                           ***
      ***===========================================================***
      *
       01  REG-REDUZIDO-TITULO.
           05 SLIG01-BCO-CLIENTE        PIC 9(003) COMP-3.
           05 SLIG01-AGENCIA-CLIENTE    PIC 9(005) COMP-3.
           05 SLIG01-CONTA-CLIENTE      PIC 9(013) COMP-3.
           05 SLIG01-CTPO-CART          PIC 9(003) COMP-3.
           05 SLIG01-CCLI-COBR          PIC 9(009) COMP-3.
           05 SLIG01-CGC-CPF-CLIENTE    PIC 9(009) COMP-3.
           05 SLIG01-FILIAL-CLIENTE     PIC 9(005) COMP-3.
           05 SLIG01-CONTROLE-CLIENTE   PIC 9(002).
           05 SLIG01-BCO-SACADO         PIC 9(003) COMP-3.
           05 SLIG01-AGENCIA-SACADO     PIC 9(005) COMP-3.
           05 SLIG01-CONTA-SACADO       PIC 9(013) COMP-3.
           05 SLIG01-CGC-CPF-SACADO     PIC 9(009) COMP-3.
           05 SLIG01-FILIAL-SACADO      PIC 9(005) COMP-3.
           05 SLIG01-CONTROLE-SACADO    PIC 9(002).
           05 SLIG01-VL-PRINC-PROD      PIC 9(017) COMP-3.
           05 SLIG01-VL-PAGO-PROD       PIC 9(015) COMP-3.
           05 SLIG01-VL-DESC-ABAT-PROD  PIC 9(015) COMP-3.
           05 SLIG01-DT-VENCTO-PROD     PIC 9(009) COMP-3.
           05 SLIG01-DT-PAGTO-PROD      PIC 9(009) COMP-3.
           05 SLIG01-DT-ENTRADA-ORIGEM  PIC 9(009) COMP-3.
           05 SLIG01-DT-ENTRADA-SISTEMA PIC 9(009) COMP-3.
           05 SLIG01-DT-VENCTO-ANTERIOR PIC 9(009) COMP-3.
           05 SLIG01-CD-PRODUTO         PIC 9(002).
           05 SLIG01-CD-CNEGOC          PIC 9(011) COMP-3.
           05 SLIG01-CD-NOSNRO          PIC 9(011) COMP-3.
           05 SLIG01-CD-SEQUENCIA       PIC 9(003) COMP-3.
           05 SLIG01-CD-COD-NTAX        PIC 9(005) COMP-3.
           05 SLIG01-CD-OCO-REM-L       PIC 9(003) COMP-3.
           05 SLIG01-CD-MOT-REM-SIS     PIC 9(003) COMP-3.
           05 SLIG01-CD-ESPECIE         PIC 9(003) COMP-3.
           05 SLIG01-CD-ORIGEM          PIC 9(003) COMP-3.
           05 SLIG01-DT-INICIO-PER      PIC 9(009) COMP-3.
           05 SLIG01-DT-FINAL-PER       PIC 9(009) COMP-3.
           05 SLIG01-TPESS-CLI          PIC X(001).
           05 SLIG01-TPESS-SAC          PIC X(001).
           05 SLIG01-CART-COBR          PIC 9(003) COMP-3.

      * === CAMPOS DUPLICADOS PARA STRING ===
           05 SLIG01-CGC-CPF-CLIENTE-ST PIC X(009).
           05 SLIG01-FILIAL-CLIENTE-ST  PIC X(005).
           05 SLIG01-CONTROLE-CLIENTE-ST PIC 9(002).
           05 SLIG01-CGC-CPF-SACADO-ST  PIC X(009).
           05 SLIG01-FILIAL-SACADO-ST   PIC X(005).
           05 SLIG01-CONTROLE-SACADO-ST PIC 9(002).
           05 SLIG01-FILLER             PIC X(008).
