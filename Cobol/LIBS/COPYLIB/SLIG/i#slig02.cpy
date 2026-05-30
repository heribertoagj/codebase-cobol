      ***===========================================================***
      *** NOME INC                                     LENGTH=0250  ***
      *** I#SLIG02                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG          ***
      ***            ARQUIVO INTERMEDIARIO DE T.TULOS               ***
      *** ARQUIVO INTERMEDIARIO - FB 0250                           ***
      ***                                                           ***
      ***            INCLUIDO CD-PRODUTO E CD-CNEGOC                ***
      ***===========================================================***
      *
       01      REG-INTERMEDIARIO-TITULO.
           03    SLIG02-CLIENTE.
             05  SLIG02-BCO-CLIENTE             PIC 9(003) COMP-3.
             05  SLIG02-AGENCIA-CLIENTE         PIC 9(005) COMP-3.
             05  SLIG02-CONTA-CLIENTE           PIC 9(013) COMP-3.
             05  SLIG02-CGC-CPF-CLIENTE         PIC 9(009) COMP-3.
             05  SLIG02-FILIAL-CLIENTE          PIC 9(005) COMP-3.
             05  SLIG02-CONTROLE-CLIENTE        PIC 9(002).
             05  SLIG02-TP-PESS-CLIENTE         PIC 9(001).
             05  SLIG02-NM-CLIENTE              PIC X(40).
             05  SLIG02-CGC-GRUPO-CLIENTE       PIC 9(009) COMP-3.
             05  SLIG02-CD-DIR-CLIENTE          PIC 9(005) COMP-3.
             05  SLIG02-CD-BCO-GER-REGIONAL     PIC 9(005) COMP-3.
             05  SLIG02-GER-REGIONAL            PIC 9(005) COMP-3.
           03    SLIG02-SACADO.
             05  SLIG02-BCO-SACADO              PIC 9(003) COMP-3.
             05  SLIG02-AGENCIA-SACADO          PIC 9(005) COMP-3.
             05  SLIG02-CONTA-SACADO            PIC 9(013) COMP-3.
             05  SLIG02-CGC-CPF-SACADO          PIC 9(009) COMP-3.
             05  SLIG02-FILIAL-SACADO           PIC 9(005) COMP-3.
             05  SLIG02-CONTROLE-SACADO         PIC 9(002).
             05  SLIG02-TP-PESS-SACADO          PIC 9(001).
             05  SLIG02-NM-SACADO               PIC X(40).
             05  SLIG02-CGC-GRUPO-SACADO        PIC 9(009) COMP-3.
             05  SLIG02-CD-DIR-SACADO           PIC 9(003) COMP-3.
           03    SLIG02-TITULO.
             05  SLIG02-CTPO-CART               PIC 9(003) COMP-3.
             05  SLIG02-CCLI-COBR               PIC 9(009) COMP-3.
             05  SLIG02-DT-INICIO-PER           PIC 9(009) COMP-3.
             05  SLIG02-DT-FINAL-PER            PIC 9(009) COMP-3.
             05  SLIG02-VANTES-PER-NAO-PG       PIC 9(013)V99 COMP-3.
             05  SLIG02-VVCTO-PER-PG            PIC 9(013)V99 COMP-3.
             05  SLIG02-VVCTO-PER-NAO-PG        PIC 9(013)V99 COMP-3.
             05  SLIG02-VVCTO-PER-BXADO         PIC 9(013)V99 COMP-3.
             05  SLIG02-VVCTO-APOS-PER-PG       PIC 9(013)V99 COMP-3.
             05  SLIG02-VVCTO-APOS-PER-NAO-PG   PIC 9(013)V99 COMP-3.
             05  SLIG02-VVCTO-APOS-PER-BXADO    PIC 9(013)V99 COMP-3.
             05  SLIG02-VTOT-PRROG              PIC 9(013)V99 COMP-3.
             05  SLIG02-VTOT-ABTMT              PIC 9(013)V99 COMP-3.
             05  SLIG02-CD-PRODUTO              PIC 9(002).
             05  SLIG02-CD-CNEGOC               PIC 9(011) COMP-3.

      * === CAMPOS DUPLICADOS PARA STRING ===
             05  SLIG02-CGC-CPF-CLIENTE-ST      PIC X(009).
             05  SLIG02-FILIAL-CLIENTE-ST       PIC X(005).
             05  SLIG02-CONTROLE-CLIENTE-ST     PIC 9(002).
             05  SLIG02-CGC-CPF-SACADO-ST       PIC X(009).
             05  SLIG02-FILIAL-SACADO-ST        PIC X(005).
             05  SLIG02-CONTROLE-SACADO-ST      PIC 9(002).
             05  SLIG02-CGC-GRUPO-CLIENTE-ST    PIC X(009).
             05  SLIG02-CGC-GRUPO-SACADO-ST     PIC X(009).
             05  SLIG02-FILLER                  PIC X(006).
