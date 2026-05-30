      ******************************************************************
      * NOME BOOK : DCOMW0C5                                           *
      * DESCRICAO : BOOK DE TRABALHO DO PROGRAMA DCOM7465.             *
      *             CAMPOS DA TABELA DCOMB0C5 - DB2PRD.TOPER_DESC_COML *
      *             OS CAMPOS FORAM CRIADOS A PARTIR DO I#DCOMC5.      *
      *             QUALQUER ALTERACACAO NO BOOK I@DCOMC5 AVALIAR A    *
      *             NECESSIDADE DE ALTERAR ESSE BOOK.                  *
      * DATA      : 27/05/2020                                         *
      * AUTOR     : LUIZ CARLOS FERREIRA                               *
      * EMPRESA   : SIS - CONSULTORIA                                  *
      * COMPONENTE:                                                    *
      * TAMANHO   : 28244 BYTES                                        *
      ******************************************************************
      *-------------------- MANUTENCOES -------------------------------*
      ******************************************************************
      * DATA       AUTOR/EMPRESA     DESCRICAO DA MANUTENCAO           *
      * 99/99/9999 AAAAAAAA/EEEEEEEE DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD *
      ******************************************************************
           05      DCOMW0C5-HEADER.
            07     DCOMW0C5-COD-LAYOUT           PIC  X(08) VALUE
                                                 'DCOMW0C5'.
            07     DCOMW0C5-TAM-LAYOUT           PIC  9(05) VALUE 28244.
           05      DCOMW0C5-REGISTRO.
      *----------------------------------------------------------------*
      * BLOCO DE ENTRADA
      *----------------------------------------------------------------*
            07     DCOMW0C5-BLOCO-ENTRADA.
             09    DCOMW0C5-I-DANO-OPER-DESC     PIC S9(4)V COMP-3.
             09    DCOMW0C5-I-NSEQ-OPER-DESC     PIC S9(9)V COMP-3.
             09    DCOMW0C5-I-CSIT-DESC-COML     PIC S9(3)V COMP-3.
             09    DCOMW0C5-I-HSIT-DESC-COML     PIC X(26).
             09    DCOMW0C5-I-VLIBRC-DESC-COML   PIC S9(15)V9(2) COMP-3.
             09    DCOMW0C5-I-VLIBRC-ANTCP-OPER  PIC S9(15)V9(2) COMP-3.
             09    DCOMW0C5-I-CBCO               PIC S9(3)V COMP-3.
             09    DCOMW0C5-I-CAG-BCRIA          PIC S9(5)V COMP-3.
             09    DCOMW0C5-I-CCTA-BCRIA-CLI     PIC S9(13)V COMP-3.
             09    DCOMW0C5-I-CBCO-OPER-OPER     PIC S9(3)V COMP-3.
             09    DCOMW0C5-I-CAG-OPER-OPER      PIC S9(5)V COMP-3.
             09    DCOMW0C5-I-CPRODT             PIC S9(3)V COMP-3.
             09    DCOMW0C5-I-CSPROD-DESC-COML   PIC S9(3)V COMP-3.
             09    DCOMW0C5-I-CTPO-DESC-COML     PIC S9(3)V COMP-3.
             09    DCOMW0C5-I-CCONTR-CONVE-DESC  PIC S9(9)V COMP-3.
             09    DCOMW0C5-I-CVRSAO-CONTR-CONVE PIC S9(3)V COMP-3.
             09    DCOMW0C5-I-CELMTO-DESC-COML   PIC S9(3)V COMP-3.
             09    DCOMW0C5-I-NSMULA-OPER-DESC   PIC S9(9)V COMP-3.
             09    DCOMW0C5-I-CINDCD-DIGTC-AG    PIC X(1).
             09    DCOMW0C5-I-CINDCD-ISENC-OPER  PIC X(1).
             09    DCOMW0C5-I-CINDCD-AUTRZ-OPER  PIC X(1).
             09    DCOMW0C5-I-CINDCD-MODLD-INCL  PIC X(1).
             09    DCOMW0C5-I-CCTRO-CUSTO        PIC X(4).
             09    DCOMW0C5-I-DANO-BASE          PIC S9(4)V COMP-3.
             09    DCOMW0C5-I-CDEPDC             PIC S9(5)V COMP-3.
             09    DCOMW0C5-I-CSEQ-STUDO         PIC S9(9)V COMP-3.
             09    DCOMW0C5-I-CINDCD-APROV-CREDT PIC X(1).
             09    DCOMW0C5-I-VOPER-DESC-COML    PIC S9(15)V9(2) COMP-3.
             09    DCOMW0C5-I-DINIC-OPER-DESC    PIC X(10).
             09    DCOMW0C5-I-DVCTO-FNAL-OPER    PIC X(10).
             09    DCOMW0C5-I-TOPER-DESC-COML    PIC S9(5)V COMP-3.
             09    DCOMW0C5-I-TMED-PONDE-OPER    PIC S9(3)V COMP-3.
             09    DCOMW0C5-I-CMEIO-ENTRD-DESC   PIC S9(3)V COMP-3.
             09    DCOMW0C5-I-QPCELA-OPER        PIC S9(5)V COMP-3.
             09    DCOMW0C5-I-QTITLO-CHEQ-PCELA  PIC S9(9)V COMP-3.
             09    DCOMW0C5-I-NAUTRZ-TRNSM-ESCR  PIC S9(7)V COMP-3.
             09    DCOMW0C5-I-CINDCD-PGDOR-JURO  PIC X(1).
             09    DCOMW0C5-I-CTPO-TX-JURO-DESC  PIC S9(3)V COMP-3.
             09    DCOMW0C5-I-PMIN-TX-JURO-REGRA PIC S9(3)V9(3) COMP-3.
             09    DCOMW0C5-I-PMAX-TX-JURO-REGRA PIC S9(3)V9(3) COMP-3.
             09    DCOMW0C5-I-PALIQT-IOF-DESC    PIC S9(3)V9(5) COMP-3.
             09    DCOMW0C5-I-PTX-JURO-MES       PIC S9(3)V9(5) COMP-3.
             09    DCOMW0C5-I-PTX-JURO-ANO       PIC S9(3)V9(5) COMP-3.
             09    DCOMW0C5-I-PTX-CUSTO-MES      PIC S9(3)V9(6) COMP-3.
             09    DCOMW0C5-I-PTX-CUSTO-ANO      PIC S9(3)V9(6) COMP-3.
             09    DCOMW0C5-I-VJURO-OPER-DESC    PIC S9(15)V9(2) COMP-3.
             09    DCOMW0C5-I-VJURO-CUSTO-OPER   PIC S9(15)V9(2) COMP-3.
             09    DCOMW0C5-I-VTARIF-REG-TITLO   PIC S9(15)V9(2) COMP-3.
             09    DCOMW0C5-I-VTAC-OPER-DESC     PIC S9(15)V9(2) COMP-3.
             09    DCOMW0C5-I-VPSENT-OPER-DESC   PIC S9(15)V9(2) COMP-3.
             09    DCOMW0C5-I-VIOF-OPER-DESC     PIC S9(15)V9(2) COMP-3.
             09    DCOMW0C5-I-VLIQ-OPER-DESC     PIC S9(15)V9(2) COMP-3.
             09    DCOMW0C5-I-CINDCD-VALDC-ROTRO PIC X(1).
             09    DCOMW0C5-I-DVALDC-ROTRO-ELMTO PIC X(10).
             09    DCOMW0C5-I-CFUNC-VALDC-ROTRO  PIC S9(9)V COMP-3.
             09    DCOMW0C5-I-DULT-ADTTO-OPER    PIC X(10).
             09    DCOMW0C5-I-CINDCD-MOTVO-ADTTO PIC X(1).
             09    DCOMW0C5-I-HULT-ATULZ         PIC X(26).
             09    DCOMW0C5-I-CFUNC-BDSCO        PIC S9(9)V COMP-3.
             09    DCOMW0C5-I-CTERM              PIC X(8).
             09    DCOMW0C5-I-CCONTR-CORP-OPER   PIC S9(7)V COMP-3.
             09    DCOMW0C5-I-DINCL-REG          PIC X(10).
             09    DCOMW0C5-I-CCART-LIM-CREDT    PIC X(05).
             09    DCOMW0C5-I-CCHAVE-ELMTO-DESC  PIC X(45).
             09    DCOMW0C5-I-CINDCD-ALIQT-REDZD PIC X(1).
             09    DCOMW0C5-I-PTX-JURO-DIA       PIC S9(3)V9(5) COMP-3.
             09    DCOMW0C5-I-CINDCD-FREQ-TX     PIC X(2).
             09    DCOMW0C5-I-CCNPJ-CPF          PIC S9(09) COMP-3.
             09    DCOMW0C5-I-CFLIAL-CNPJ        PIC S9(05) COMP-3.
             09    DCOMW0C5-I-CCTRL-CNPJ-CPF     PIC S9(02) COMP-3.


      *----------------------------------------------------------------*
      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *----------------------------------------------------------------*
             09    DCOMW0C5-I-QTDE-A-DESPREZAR   PIC 9(05).

      *----------------------------------------------------------------*
      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *----------------------------------------------------------------*
             09    DCOMW0C5-I-INSTRUCAO          PIC X(10).

      *----------------------------------------------------------------*
      * SERAO PREENCHIDOS NO CASO DE FETCH
      *----------------------------------------------------------------*
             09    DCOMW0C5-I-PROXIMO-RESTART    PIC 9(05).
             09    DCOMW0C5-I-QTDE-RETORNADA     PIC 9(03).
             09    DCOMW0C5-I-QTDE-TOTAL         PIC 9(09).


            07     DCOMW0C5-BLOCO-SAIDA.
      *----------------------------------------------------------------*
      * BLOCO DE ENTRADA
      *----------------------------------------------------------------*
             09    DCOMW0C5-S-COD-RETORNO        PIC 9(04).
             09    DCOMW0C5-S-MENSAGEM           PIC X(79).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
             09    DCOMW0C5-S-OCORRENCIAS OCCURS 070 TIMES
                                          INDEXED BY IND-0C5.
              11   DCOMW0C5-S-DANO-OPER-DESC     PIC S9(4)V COMP-3.
              11   DCOMW0C5-S-NSEQ-OPER-DESC     PIC S9(9)V COMP-3.
              11   DCOMW0C5-S-CSIT-DESC-COML     PIC S9(3)V COMP-3.
              11   DCOMW0C5-S-HSIT-DESC-COML     PIC X(26).
              11   DCOMW0C5-S-VLIBRC-DESC-COML   PIC S9(15)V9(2) COMP-3.
              11   DCOMW0C5-S-VLIBRC-ANTCP-OPER  PIC S9(15)V9(2) COMP-3.
              11   DCOMW0C5-S-CBCO               PIC S9(3)V COMP-3.
              11   DCOMW0C5-S-CAG-BCRIA          PIC S9(5)V COMP-3.
              11   DCOMW0C5-S-CCTA-BCRIA-CLI     PIC S9(13)V COMP-3.
              11   DCOMW0C5-S-CBCO-OPER-OPER     PIC S9(3)V COMP-3.
              11   DCOMW0C5-S-CAG-OPER-OPER      PIC S9(5)V COMP-3.
              11   DCOMW0C5-S-CPRODT             PIC S9(3)V COMP-3.
              11   DCOMW0C5-S-CSPROD-DESC-COML   PIC S9(3)V COMP-3.
              11   DCOMW0C5-S-CTPO-DESC-COML     PIC S9(3)V COMP-3.
              11   DCOMW0C5-S-CCONTR-CONVE-DESC  PIC S9(9)V COMP-3.
              11   DCOMW0C5-S-CVRSAO-CONTR-CONVE PIC S9(3)V COMP-3.
              11   DCOMW0C5-S-CELMTO-DESC-COML   PIC S9(3)V COMP-3.
              11   DCOMW0C5-S-NSMULA-OPER-DESC   PIC S9(9)V COMP-3.
              11   DCOMW0C5-S-CINDCD-DIGTC-AG    PIC X(1).
              11   DCOMW0C5-S-CINDCD-ISENC-OPER  PIC X(1).
              11   DCOMW0C5-S-CINDCD-AUTRZ-OPER  PIC X(1).
              11   DCOMW0C5-S-CINDCD-MODLD-INCL  PIC X(1).
              11   DCOMW0C5-S-CCTRO-CUSTO        PIC X(4).
              11   DCOMW0C5-S-DANO-BASE          PIC S9(4)V COMP-3.
              11   DCOMW0C5-S-CDEPDC             PIC S9(5)V COMP-3.
              11   DCOMW0C5-S-CSEQ-STUDO         PIC S9(9)V COMP-3.
              11   DCOMW0C5-S-CINDCD-APROV-CREDT PIC X(1).
              11   DCOMW0C5-S-VOPER-DESC-COML    PIC S9(15)V9(2) COMP-3.
              11   DCOMW0C5-S-DINIC-OPER-DESC    PIC X(10).
              11   DCOMW0C5-S-DVCTO-FNAL-OPER    PIC X(10).
              11   DCOMW0C5-S-TOPER-DESC-COML    PIC S9(5)V COMP-3.
              11   DCOMW0C5-S-TMED-PONDE-OPER    PIC S9(3)V COMP-3.
              11   DCOMW0C5-S-CMEIO-ENTRD-DESC   PIC S9(3)V COMP-3.
              11   DCOMW0C5-S-QPCELA-OPER        PIC S9(5)V COMP-3.
              11   DCOMW0C5-S-QTITLO-CHEQ-PCELA  PIC S9(9)V COMP-3.
              11   DCOMW0C5-S-NAUTRZ-TRNSM-ESCR  PIC S9(7)V COMP-3.
              11   DCOMW0C5-S-CINDCD-PGDOR-JURO  PIC X(1).
              11   DCOMW0C5-S-CTPO-TX-JURO-DESC  PIC S9(3)V COMP-3.
              11   DCOMW0C5-S-PMIN-TX-JURO-REGRA PIC S9(3)V9(3) COMP-3.
              11   DCOMW0C5-S-PMAX-TX-JURO-REGRA PIC S9(3)V9(3) COMP-3.
              11   DCOMW0C5-S-PALIQT-IOF-DESC    PIC S9(3)V9(5) COMP-3.
              11   DCOMW0C5-S-PTX-JURO-MES       PIC S9(3)V9(5) COMP-3.
              11   DCOMW0C5-S-PTX-JURO-ANO       PIC S9(3)V9(5) COMP-3.
              11   DCOMW0C5-S-PTX-CUSTO-MES      PIC S9(3)V9(6) COMP-3.
              11   DCOMW0C5-S-PTX-CUSTO-ANO      PIC S9(3)V9(6) COMP-3.
              11   DCOMW0C5-S-VJURO-OPER-DESC    PIC S9(15)V9(2) COMP-3.
              11   DCOMW0C5-S-VJURO-CUSTO-OPER   PIC S9(15)V9(2) COMP-3.
              11   DCOMW0C5-S-VTARIF-REG-TITLO   PIC S9(15)V9(2) COMP-3.
              11   DCOMW0C5-S-VTAC-OPER-DESC     PIC S9(15)V9(2) COMP-3.
              11   DCOMW0C5-S-VPSENT-OPER-DESC   PIC S9(15)V9(2) COMP-3.
              11   DCOMW0C5-S-VIOF-OPER-DESC     PIC S9(15)V9(2) COMP-3.
              11   DCOMW0C5-S-VLIQ-OPER-DESC     PIC S9(15)V9(2) COMP-3.
              11   DCOMW0C5-S-CINDCD-VALDC-ROTRO PIC X(1).
              11   DCOMW0C5-S-DVALDC-ROTRO-ELMTO PIC X(10).
              11   DCOMW0C5-S-CFUNC-VALDC-ROTRO  PIC S9(9)V COMP-3.
              11   DCOMW0C5-S-DULT-ADTTO-OPER    PIC X(10).
              11   DCOMW0C5-S-CINDCD-MOTVO-ADTTO PIC X(1).
              11   DCOMW0C5-S-HULT-ATULZ         PIC X(26).
              11   DCOMW0C5-S-CFUNC-BDSCO        PIC S9(9)V COMP-3.
              11   DCOMW0C5-S-CTERM              PIC X(8).
              11   DCOMW0C5-S-CCONTR-CORP-OPER   PIC S9(7)V COMP-3.
              11   DCOMW0C5-S-DINCL-REG          PIC X(10).
              11   DCOMW0C5-S-CCART-LIM-CREDT    PIC X(05).
              11   DCOMW0C5-S-CCHAVE-ELMTO-DESC  PIC X(45).
              11   DCOMW0C5-S-CINDCD-ALIQT-REDZD PIC X(1).
              11   DCOMW0C5-S-PTX-JURO-DIA       PIC S9(3)V9(5) COMP-3.
              11   DCOMW0C5-S-CINDCD-FREQ-TX     PIC X(2).
