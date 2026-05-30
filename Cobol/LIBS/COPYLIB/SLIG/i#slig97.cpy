      ***===========================================================***
      *** NOME INC                                     LENGTH=0200  ***
      *** I#SLIG97                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG          ***
      ***            ARQUIVO REDUZIDO INTERMEDIARIO ENTRE AS        ***
      ***            INCLUDES I#SLIG01 E I#SLIG02                   ***
      ***                                                           ***
      *** OBSERVACAO: O CAMPO SLIG97-CD-ORIG-NM-SAC INDICA A        ***
      ***             ORIGEM DO NOME DO SACADO, SENDO:              ***
      ***                                                           ***
      ***             0 - NOME NAO ENCONTRADO;                      ***
      ***             1 - NOME ENCONTRADO NO CADACLIE;              ***
      ***             2 - NOME ENCONTRADO NA COBRANCA/DESCONTO;     ***
      ***                                                           ***
      ***===========================================================***
      *
       01  REG-INTERMED-SACADO.
           05 SLIG97-BCO-CLIENTE        PIC 9(003) COMP-3.
           05 SLIG97-AGENCIA-CLIENTE    PIC 9(005) COMP-3.
           05 SLIG97-CONTA-CLIENTE      PIC 9(013) COMP-3.
           05 SLIG97-CTPO-CART          PIC 9(003) COMP-3.
           05 SLIG97-CCLI-COBR          PIC 9(009) COMP-3.
           05 SLIG97-CGC-CPF-CLIENTE    PIC 9(009) COMP-3.
           05 SLIG97-FILIAL-CLIENTE     PIC 9(005) COMP-3.
           05 SLIG97-CONTROLE-CLIENTE   PIC 9(002).
           05 SLIG97-BCO-SACADO         PIC 9(003) COMP-3.
           05 SLIG97-AGENCIA-SACADO     PIC 9(005) COMP-3.
           05 SLIG97-CONTA-SACADO       PIC 9(013) COMP-3.
           05 SLIG97-CGC-CPF-SACADO     PIC 9(009) COMP-3.
           05 SLIG97-FILIAL-SACADO      PIC 9(005) COMP-3.
           05 SLIG97-CONTROLE-SACADO    PIC 9(002).
           05 SLIG97-NOME-SACADO        PIC X(040).
           05 SLIG97-CD-ORIG-NM-SAC     PIC 9(001).
           05 SLIG97-VL-PRINC-PROD      PIC 9(017) COMP-3.
           05 SLIG97-VL-PAGO-PROD       PIC 9(015) COMP-3.
           05 SLIG97-VL-DESC-ABAT-PROD  PIC 9(015) COMP-3.
           05 SLIG97-DT-VENCTO-PROD     PIC 9(009) COMP-3.
           05 SLIG97-DT-PAGTO-PROD      PIC 9(009) COMP-3.
           05 SLIG97-DT-ENTRADA-ORIGEM  PIC 9(009) COMP-3.
           05 SLIG97-DT-ENTRADA-SISTEMA PIC 9(009) COMP-3.
           05 SLIG97-DT-VENCTO-ANTERIOR PIC 9(009) COMP-3.
           05 SLIG97-CD-PRODUTO         PIC 9(002).
           05 SLIG97-CD-CNEGOC          PIC 9(011) COMP-3.
           05 SLIG97-CD-NOSNRO          PIC 9(011) COMP-3.
           05 SLIG97-CD-SEQUENCIA       PIC 9(003) COMP-3.
           05 SLIG97-CD-COD-NTAX        PIC 9(005) COMP-3.
           05 SLIG97-CD-OCO-REM-L       PIC 9(003) COMP-3.
           05 SLIG97-CD-MOT-REM-SIS     PIC 9(003) COMP-3.
           05 SLIG97-CD-ESPECIE         PIC 9(003) COMP-3.
           05 SLIG97-CD-ORIGEM          PIC 9(003) COMP-3.
           05 SLIG97-DT-INICIO-PER      PIC 9(009) COMP-3.
           05 SLIG97-DT-FINAL-PER       PIC 9(009) COMP-3.

      * === CAMPOS DUPLICADOS PARA STRING ===
           05 SLIG97-CGC-CPF-CLIENTE-ST PIC X(009).
           05 SLIG97-FILIAL-CLIENTE-ST  PIC X(005).
           05 SLIG97-CONTROLE-CLIENTE-ST PIC 9(002).
           05 SLIG97-CGC-CPF-SACADO-ST  PIC X(009).
           05 SLIG97-FILIAL-SACADO-ST   PIC X(005).
           05 SLIG97-CONTROLE-SACADO-ST PIC 9(002).
           05 SLIG97-FILLER             PIC X(021).
