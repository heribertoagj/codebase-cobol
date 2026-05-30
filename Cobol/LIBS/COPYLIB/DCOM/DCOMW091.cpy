      ******************************************************************
      * NOME BOOK : DCOMW091                                           *
      * DESCRICAO : BOOK DE TRABALHO DO PROGRAMA DCOM7466.             *
      *             CAMPOS DA TABELA DCOMB091 - DB2PRD.TSPROD_DESC_COML*
      *             OS CAMPOS FORAM CRIADOS A PARTIR DO I#DCOM91.      *
      *             QUALQUER ALTERACACAO NO BOOK I@DCOM91 AVALIAR A    *
      *             NECESSIDADE DE ALTERAR ESSE BOOK.                  *
      *             A QUANTIDADE DE OCORRENCIA FOI DIMINUIDA PARA 45   *
      *             PARA EVITAR ERRO DE COMUNICACAO NO ON-LINE.        *
      * DATA      : 27/05/2020                                         *
      * AUTOR     : LUIZ CARLOS FERREIRA                               *
      * EMPRESA   : SIS - CONSULTORIA                                  *
      * COMPONENTE:                                                    *
      * TAMANHO   : 29578 BYTES                                        *
      ******************************************************************
      *-------------------- MANUTENCOES -------------------------------*
      ******************************************************************
      * DATA       AUTOR/EMPRESA     DESCRICAO DA MANUTENCAO           *
      * 99/99/9999 AAAAAAAA/EEEEEEEE DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD *
      ******************************************************************
           05      DCOMW091-HEADER.
            07     DCOMW091-COD-LAYOUT           PIC  X(08) VALUE
                                                 'DCOMW091'.
            07     DCOMW091-TAM-LAYOUT           PIC  9(05) VALUE 29578.
           05      DCOMW091-REGISTRO.
      *----------------------------------------------------------------*
      * BLOCO DE ENTRADA
      *----------------------------------------------------------------*
            07     DCOMW091-BLOCO-ENTRADA.
             09    DCOMW091-I-CPRODT             PIC S9(3)V COMP-3.
             09    DCOMW091-I-CSPROD-DESC-COML   PIC S9(3)V COMP-3.
             09    DCOMW091-I-DINIC-VGCIA-SPROD  PIC X(10).
             09    DCOMW091-I-DFIM-VGCIA-SPROD   PIC X(10).
             09    DCOMW091-I-CTPO-DESC-COML     PIC S9(3)V COMP-3.
             09    DCOMW091-I-CSIT-DESC-COML     PIC S9(3)V COMP-3.
             09    FILLER                        PIC X(50).

      *----------------------------------------------------------------*
      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *----------------------------------------------------------------*
             09    DCOMW091-I-QTDE-A-DESPREZAR   PIC 9(05).

      *----------------------------------------------------------------*
      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *----------------------------------------------------------------*
             09    DCOMW091-I-INSTRUCAO          PIC X(10).

      *----------------------------------------------------------------*
      * SERAO PREENCHIDOS NO CASO DE FETCH
      *----------------------------------------------------------------*
             09    DCOMW091-I-PROXIMO-RESTART    PIC 9(05).
             09    DCOMW091-I-QTDE-RETORNADA     PIC 9(03).
             09    DCOMW091-I-QTDE-TOTAL         PIC 9(09).

            07     DCOMW091-BLOCO-SAIDA.
      *----------------------------------------------------------------*
      * BLOCO DE ENTRADA
      *----------------------------------------------------------------*
             09    DCOMW091-S-COD-RETORNO        PIC 9(04).
             09    DCOMW091-S-MENSAGEM           PIC X(79).

      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
             09    DCOMW091-S-OCORRENCIAS OCCURS 045 TIMES
                                          INDEXED BY IND-0C5.
              11   DCOMW091-S-CPRODT             PIC S9(3)V COMP-3.
              11   DCOMW091-S-CSPROD-DESC-COML   PIC S9(3)V COMP-3.
              11   DCOMW091-S-DINIC-VGCIA-SPROD  PIC X(10).
              11   DCOMW091-S-DFIM-VGCIA-SPROD   PIC X(10).
              11   DCOMW091-S-CTPO-DESC-COML     PIC S9(3)V COMP-3.
              11   DCOMW091-S-ISPROD-DESC-COML   PIC X(40).
              11   DCOMW091-S-IRSUMO-SPROD-DESC  PIC X(10).
              11   DCOMW091-S-CSGL-SPROD-DESC    PIC X(6).
              11   DCOMW091-S-CSIT-DESC-COML     PIC S9(3)V COMP-3.
              11   DCOMW091-S-HSIT-DESC-COML     PIC X(26).
              11   DCOMW091-S-CINDCD-VALDC-ROTRO PIC X(1).
              11   DCOMW091-S-DVALDC-ROTRO-ELMTO PIC X(10).
              11   DCOMW091-S-CFUNC-VALDC-ROTRO  PIC S9(9)V COMP-3.
              11   DCOMW091-S-RSPROD-DESC-COML.
               13  DCOMW091-S-RSPROD-DESC-COML-L
                                                 PIC S9(4) COMP.
               13  DCOMW091-S-RSPROD-DESC-COML-T
                                                 PIC X(240).
              11   DCOMW091-S-CPRODT-BASE-INCL   PIC S9(3)V COMP-3.
              11   DCOMW091-S-CSPROD-BASE-INCL   PIC S9(3)V COMP-3.
              11   DCOMW091-S-RJUSTF-SIT-SPROD.
               13  DCOMW091-S-RJUSTF-SIT-SPROD-L
                                                 PIC S9(4) COMP.
               13  DCOMW091-S-RJUSTF-SIT-SPROD-T
                                                 PIC X(240).
              11   DCOMW091-S-HULT-ATULZ         PIC X(26).
              11   DCOMW091-S-CFUNC-BDSCO        PIC S9(9)V COMP-3.
              11   DCOMW091-S-CTERM              PIC X(8).
