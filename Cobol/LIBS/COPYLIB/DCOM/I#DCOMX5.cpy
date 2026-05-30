      *********************************************************
      * SISTEMA   : DCOM - DESCONTO COMERCIAL  NOME: I#DCOMX5 *
      * CRIACAO   : 08/2006                                   *
      * DESCRICAO : ENQUADRAR E CALCULAR TAXA DE JUROS        *
      * APLICACAO : MODULO DCOM5364                           *
      *********************************************************
DTS001*              ALTERACAO -  DTS  CONSULTING             *
DTS001*-------------------------------------------------------*
DTS001* PROGRAMADORA: LUCIANDRA SILVEIRA - DTS CONSULTING     *
DTS001* ANALISTA DDS: EDGARD R. DE SOUZA - BRADESCO - GP. 70  *
DTS001* DATA........: 16/07/2012                              *
DTS001*-------------------------------------------------------*
DTS001* OBJETIVO....: INCLUIR NIVEL DE RATING.                *
DTS001*               PMO 12/0129 - NIVEL DE RATING.          *
DTS001*=======================================================*

      *********************************************************
      * AREA DE ENTRADA                                       *
      *********************************************************

       01  WRK-ENVIO-5364.
           03 WRK-CINDCD-PSSOA-TOMAD-5364    PIC  X(01).
           03 WRK-CSGMTO-CLI-5364            PIC  9(03).
           03 WRK-CRTING-5364                PIC  X(03).
DTS001     03 WRK-NNVEL-RTING-5364           PIC  9(02).
DTS001     03 WRK-HINIC-VGCIA-NVEL-5364      PIC  X(26).
           03 WRK-CPRODT-5364                PIC  9(03).
           03 WRK-CSPROD-DESC-COML-5364      PIC  9(03).
           03 WRK-CMEIO-ENTRD-DESC-5364      PIC  9(03).
           03 WRK-CELMTO-DESC-COML-5364      PIC  9(03).
           03 WRK-CCONTR-CONVE-DESC-5364     PIC  9(09).
           03 WRK-CVRSAO-CONTR-CONVE-5364    PIC  9(03).
           03 WRK-DINIC-OPER-DESC-5364       PIC  X(10).
           03 WRK-VOPER-DESC-COML-5364       PIC  S9(15)V9(2).
           03 WRK-TMED-PONDE-OPER-5364       PIC  9(03).
WIP001     03 WRK-FLAG-5364                  PIC  X(001).
WIP001     03 FILLER                         PIC  X(099).

      *********************************************************
      * AREA DE RETORNO                                       *
      *********************************************************

       01  WRK-RETORNO-5364.
           03 WRK-COD-RET-5364               PIC  9(04).
           03 WRK-MENS-RET-5364              PIC  X(79).
           03 WRK-CTPO-TX-JURO-DESC-5364     PIC  9(03).
           03 WRK-CSGL-TPO-TX-JURO-5364      PIC  X(06).
           03 WRK-PMIN-TX-JURO-5364          PIC  S9(06)V9(3).
           03 WRK-PMAX-TX-JURO-5364          PIC  S9(06)V9(3).
           03 WRK-PTX-JURO-DFNDO-5364        PIC  S9(06)V9(3).
           03 WRK-PTX-JURO-CDI-5364          PIC  S9(06)V9(3).
           03 WRK-CINDCD-FREQ-TX-5364        PIC  X(02).
DTS001     03 FILLER                         PIC  X(100).
