      ***===========================================================***
      *** NOME INC                                     LENGTH=352   ***
      *** I#SLIG24                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG          ***
      ***            AREA DE COMUNICAO DO MODULO PARA               ***
      ***            CALCULO INDICES DE LIQUIDEZ CHEQUES            ***
      ***===========================================================***
      *
       01 REG-INDICE-CHEQUES.
          03 SLIG24-DADOS-ENVIADOS.
             05 SLIG24-DADOS-COMUNS.
                07 SLIG24-CD-CALC                 PIC  X(01).
             05 SLIG24-DADOS-NIVEL-DETALHE.
                07 SLIG24-DET-VAPOS-PER-PG        PIC S9(15)V99 COMP-3.
                07 SLIG24-DET-VAPOS-PER-NAO-PG    PIC S9(15)V99 COMP-3.
                07 SLIG24-DET-VAPOS-PER-BXADO     PIC S9(15)V99 COMP-3.
                07 SLIG24-DET-VVCTO-PER-BXADO     PIC S9(15)V99 COMP-3.
                07 SLIG24-DET-VVCTO-PER-PG        PIC S9(15)V99 COMP-3.
                07 SLIG24-DET-VVCTO-PER-NAO-PG    PIC S9(15)V99 COMP-3.
                07 SLIG24-DET-VCHEQ-PRIM-DEVLC    PIC S9(15)V99 COMP-3.
                07 SLIG24-DET-VCHEQ-REAP          PIC S9(15)V99 COMP-3.
                07 SLIG24-DET-VCHEQ-REAP-PG       PIC S9(15)V99 COMP-3.
                07 SLIG24-DET-VCHEQ-REAP-DEVOL    PIC S9(15)V99 COMP-3.
                07 SLIG24-DET-VCHEQ-DEVOL-ANTER   PIC S9(15)V99 COMP-3.
                07 FILLER                         PIC X(18).
             05 SLIG24-DADOS-NIVEL-TOTAL.
                07 SLIG24-TOT-VAPOS-PER-PG        PIC S9(15)V99 COMP-3.
                07 SLIG24-TOT-VAPOS-PER-NAO-PG    PIC S9(15)V99 COMP-3.
                07 SLIG24-TOT-VAPOS-PER-BXADO     PIC S9(15)V99 COMP-3.
                07 SLIG24-TOT-VVCTO-PER-BXADO     PIC S9(15)V99 COMP-3.
                07 SLIG24-TOT-VVCTO-PER-PG        PIC S9(15)V99 COMP-3.
                07 SLIG24-TOT-VVCTO-PER-NAO-PG    PIC S9(15)V99 COMP-3.
                07 SLIG24-TOT-VCHEQ-PRIM-DEVLC    PIC S9(15)V99 COMP-3.
                07 SLIG24-TOT-VCHEQ-REAP          PIC S9(15)V99 COMP-3.
                07 SLIG24-TOT-VCHEQ-REAP-PG       PIC S9(15)V99 COMP-3.
                07 SLIG24-TOT-VCHEQ-REAP-DEVOL    PIC S9(15)V99 COMP-3.
                07 SLIG24-TOT-VCHEQ-DEVOL-ANTER   PIC S9(15)V99 COMP-3.
                07 FILLER                         PIC X(18).
          03 SLIG24-DADOS-RETORNADOS.
             05 SLIG24-INDICES.
                07 SLIG24-INDCE-LIQDZ-FINAL       PIC S9(03)V99 COMP-3.
                07 SLIG24-ILF-SEM-INDICE          PIC  X(01).
                07 SLIG24-INDCE-LIQDZ-PRIM-DEPOS  PIC S9(03)V99 COMP-3.
                07 SLIG24-ILPD-SEM-INDICE         PIC  X(01).
                07 SLIG24-INDCE-LIQDZ-REAPR       PIC S9(03)V99 COMP-3.
                07 SLIG24-ILR-SEM-INDICE          PIC  X(01).
                07 SLIG24-INDCE-BX-ANTCP          PIC S9(03)V99 COMP-3.
                07 SLIG24-IBA-SEM-INDICE          PIC  X(01).
                07 SLIG24-INDCE-VCTO-PER          PIC S9(03)V99 COMP-3.
                07 SLIG24-IVP-SEM-INDICE          PIC  X(01).
                07 SLIG24-INDCE-CHEQ-DEPOS        PIC S9(03)V99 COMP-3.
                07 SLIG24-ICD-SEM-INDICE          PIC  X(01).
                07 SLIG24-INDCE-CHEQ-REAPR        PIC S9(03)V99 COMP-3.
                07 SLIG24-ICR-SEM-INDICE          PIC  X(01).
                07 SLIG24-INDCE-CONCENTR          PIC S9(03)V99 COMP-3.
                07 SLIG24-IC-SEM-INDICE           PIC  X(01).
             05 SLIG24-TOTAIS.
                07 SLIG24-TOT-CART-TOTAL          PIC S9(15)V99 COMP-3.
                07 SLIG24-TOT-CART-DETALHE        PIC S9(15)V99 COMP-3.
                07 SLIG24-TOT-VCTO-CART-TOTAL     PIC S9(15)V99 COMP-3.
                07 SLIG24-TOT-VCTO-CART-DETALHE   PIC S9(15)V99 COMP-3.
                07 FILLER                         PIC X(18).
             05 SLIG24-CONTROLE-RETORNO.
                07 SLIG24-COD-RETORNO             PIC S9(01)    COMP-3.
                07 SLIG24-NOME-CAMPO-ERRO         PIC  X(30).
