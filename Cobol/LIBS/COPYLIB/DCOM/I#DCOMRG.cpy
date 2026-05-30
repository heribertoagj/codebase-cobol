      ******************************************************************
      * SISTEMA : DCOM - I#DCOMRG                                      *
      * ***   COPIA DA INC I#CLLPIO COM ANO, SEQUENCIA E PARCELA   *** *
BSI001* TAMANHO : 0352 BYTES                                           *
      * ARQUIVO : PARCELAS VENCIDAS E BAIXADAS A SEREM ENVIADAS PARA   *
      *           O CLLP                                               *
      ******************************************************************
BSI001*                    ALTERACAO - BSI TECNOLOGIA                  *
BSI001*----------------------------------------------------------------*
BSI001*  ANALISTA BSI:   FERNANDO LUIZ DE SANTI  -  BSI TECNOLOGIA     *
BSI001*  DATA........:   28/04/2017                                    *
BSI001*----------------------------------------------------------------*
BSI001*  OBJETIVO....:   INCLUIR CAMPOS DE ENCARGOS MORATORIOS.        *
BSI001******************************************************************

       01  REG-BAIXA.
           05  RG-BX-CHAVE.
               10 RG-BX-EMPRESA        PIC  9(05) COMP-3.
               10 RG-BX-AGENCIA        PIC  9(05) COMP-3.
               10 RG-BX-NUMCL          PIC  9(15) COMP-3.
               10 RG-BX-DIGITO         PIC  X(01).
           05  RG-BX-DATA-BAIXA        PIC  X(10).
           05  RG-BX-CONTA-CORR.
               10 RG-BX-NUM-CC         PIC  9(07) COMP-3.
               10 RG-BX-DIG-CC         PIC  X(01).
           05  RG-BX-CARTEIRA          PIC  X(03).
           05  RG-BX-CONTRATO          PIC  9(07) COMP-3.
           05  RG-BX-ID                PIC  X(02).
           05  RG-BX-TIPO-BAIXA        PIC  X(01).
           05  RG-BX-DEVEDOR.
               10 RG-BX-NOME           PIC  X(40).
               10 RG-BX-CGC-CPF.
                  15 RG-BX-NUM-CGC-CPF PIC  9(09) COMP-3.
                  15 RG-BX-FIL-CGC-CPF PIC  9(05) COMP-3.
                  15 RG-BX-CTR-CGC-CPF PIC  9(02).
           05  RG-BX-DATA-VENCTO       PIC  X(10).
           05  RG-BX-DATA-CL           PIC  X(10).
           05  RG-BX-DATA-LP           PIC  X(10).
           05  RG-BX-VR-CONTABIL       PIC  9(13)V9(02) COMP-3.
           05  RG-BX-VR-DEV-INICIAL    PIC  9(13)V9(02) COMP-3.
           05  RG-BX-VR-BASE           PIC  9(13)V9(02) COMP-3.
           05  RG-BX-ENCARGO-VENCIDO   PIC  9(13)V9(02) COMP-3.
           05  RG-BX-ENCARGO-VINCENDO  PIC  9(13)V9(02) COMP-3.
           05  RG-BX-DEBITO-CC         PIC  9(13)V9(02) COMP-3.
           05  RG-BX-PRINCIPAL         PIC  9(13)V9(02) COMP-3.
           05  RG-BX-JUROS-MORA        PIC  9(13)V9(02) COMP-3.
           05  RG-BX-CORR-MONETARIA    PIC  9(13)V9(02) COMP-3.
           05  RG-BX-JUROS-12PAA       PIC  9(13)V9(02) COMP-3.
           05  RG-BX-COBRANCA-DIARIO   PIC  9(13)V9(02) COMP-3.
           05  RG-BX-TOTAL-CONTRATO    PIC  9(13)V9(02) COMP-3.
           05  RG-BX-COD-EMPRESA       PIC  X(02).
           05  RG-BX-COD-DIR-REGIONAL  PIC  9(05) COMP-3.
           05  RG-BX-DT-CLPDD-180      PIC  X(10).
           05  RG-BX-MARCA-PDD-180     PIC  X(01).
           05  RG-BX-RAZAO-PRINCIP     PIC  9(05) COMP-3.
           05  RG-BX-RAZAO-RENDAS      PIC  9(05) COMP-3.
           05  RG-BX-COD-NAT-OPER      PIC  X(03).
           05  RG-BX-MOEDA             PIC  X(02).
           05  RG-BX-TIPO-GARANTIA     PIC  X(02).
           05  RG-BX-LOCAL             PIC  X(02).
           05  RG-BX-TIPO-PENDENCI     PIC  9(05) COMP-3.
           05  RG-BX-MARCA-IMPE        PIC  X(01).
           05  RG-BX-DATA-AJUIZAMENTO  PIC  X(10).
           05  RG-BX-IOF-NORMAL        PIC  9(13)V9(02) COMP-3.
           05  RG-BX-IOF-COMPL         PIC  9(13)V9(02) COMP-3.
           05  RG-BX-ALIQ-CONT-RECOL   PIC  9(03)V9(06) COMP-3.
           05  RG-BX-ALIQ-REC-TR-CL    PIC  9(03)V9(06) COMP-3.
           05  RG-BX-ALIQ-COMPL        PIC  9(03)V9(06) COMP-3.
           05  RG-BX-IOF-OPCAO         PIC  X(01).
           05  RG-BX-VR-BASE-IOF       PIC  9(13)V9(02) COMP-3.
           05  RG-BX-DATA-SISTEL       PIC  X(10).
           05  RG-BX-IND-SISTEL        PIC  X(01).
           05  RG-BX-NOTIF-SISTEL      PIC  X(01).
           05  RG-BX-BAIXA-COMPL       PIC  X(01).
           05  RG-BX-DANO-OPER-DESC    PIC S9(4)V USAGE COMP-3.
           05  RG-BX-NSEQ-OPER-DESC    PIC S9(9)V USAGE COMP-3.
           05  RG-BX-NPCELA-DESC-COML  PIC S9(5)V USAGE COMP-3.
           05  RG-BX-VJURO-REMUN-VENCD  PIC S9(15)V9(2) USAGE COMP-3.
           05  RG-BX-VMORA-VENCD-DESC   PIC S9(15)V9(2) USAGE COMP-3.
           05  RG-BX-VMULTA-VENCD-DESC PIC S9(15)V9(2) USAGE COMP-3.
