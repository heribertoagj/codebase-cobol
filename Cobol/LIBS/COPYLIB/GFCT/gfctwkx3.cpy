      *****************************************************************
      * NOME DA INC - GFCTWKX3                                        *
      * DESCRICAO   - MOVIMENTO CESTA EXCLUSIVE PARA ENCARTEIRAMENTO  *
      *               OCORRENCIAS ROTINA MENSAL (ISD)                 *
      * TAMANHO     - 064                                             *
      * DATA        - NOV/2016                                        *
      * RESPONSAVEL - PAGNOCCA - SONDA IT                             *
      *================================================================*
ST2507*               U L T I M A   A L T E R A C A O                *
      *----------------------------------------------------------------*
ST2507*    ANALISTA....:  STEFANINI                                    *
ST2507*    DATA........:  07 / 2025                                    *
ST2507*    OBJETIVO....:  CNPJ ALFANUMERICO                            *
      *================================================================*
      *
       01  GFCTWKX3-REG.
ST2507*    10 KX3-PRINCIPAL-CPF        PIC S9(09)V              COMP-3.
ST2507*    10 KX3-FILIAL-CPF           PIC S9(05)V              COMP-3.
ST2507     10 KX3-PRINCIPAL-CPF        PIC  X(09).
ST2507     10 KX3-FILIAL-CPF           PIC  X(04).
           10 KX3-CTR-CPF              PIC S9(02)V              COMP-3.
           10 KX3-CJUNC-DEPDC          PIC S9(05)V              COMP-3.
           10 KX3-CCTA-CLI             PIC S9(07)V              COMP-3.
           10 KX3-CSERVC-TARIF         PIC S9(05)V              COMP-3.
           10 KX3-PER-ISENC            PIC S9(03)V              COMP-3.
           10 KX3-DATA-ENCART          PIC S9(09)V              COMP-3.
           10 KX3-DATA-ABERT-CTA       PIC S9(09)V              COMP-3.
           10 KX3-CD-SEGMENTO          PIC S9(03)V              COMP-3.
           10 KX3-AAAAMM-COMPET        PIC S9(06)V              COMP-3.
           10 KX3-TP-DESCONTO          PIC S9(01)V              COMP-3.
           10 KX3-VR-A-COBRAR          PIC S9(09)V9(02)         COMP-3.
           10 KX3-CSERVC-TARIF-ANT     PIC S9(05)V              COMP-3.
           10 KX3-SIT-ATUALIZ          PIC S9(03)V              COMP-3.
           10 KX3-AAAAMM-ISENC-DESC    PIC S9(06)V              COMP-3.
ST2507*    10 KX3-RESERVA              PIC  X(10).
ST2507     10 KX3-RESERVA              PIC  X(05).
      *
      *****************************************************************
      *
