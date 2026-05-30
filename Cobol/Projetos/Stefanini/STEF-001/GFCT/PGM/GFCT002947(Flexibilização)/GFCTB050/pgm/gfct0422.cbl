      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT0422.
       AUTHOR.     ROBSON FAUSTO CESA.
      *================================================================*
      *                    C P M  -  S I S T E M A S                   *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT0422                                    *
      *    PROGRAMADOR.:   ROBSON FAUSTO CESA      - CPM PATO BRANCO   *
      *    ANALISTA CPM:   JOSSIANE CARLA GAZZONI  - CPM PATO BRANCO   *
      *    ANALISTA....:   VALERIA                 - PROCWORK /GP.50   *
      *    DATA........:   05/05/2006                                  *
      *                                                                *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   FLEXIBILIZACAO - LISTA DE CONSULTA DE FLEXI-*
      *      BILIZACOES VIGENTES POR AGRUPAMENTO.                      *
      *                                                                *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *                DB2                                             *
      *                  TABLE                        INCLUDE/BOOK     *
      *                   DB2PRD.V01UF                  DCITV002       *
      *                   DB2PRD.TTPO_SGMTO_GSTAO       GFCTB0H5       *
      *                   DB2PRD.PARM_DATA_PROCM        GFCTB0A1       *
      *                   DB2PRD.SERVC_TARIF_PRINC      GFCTB0D8       *
      *                   DB2PRD.FLEXZ_GRP_UF           GFCTB057       *
      *                   DB2PRD.FLEXZ_GRP_MUN          GFCTB053       *
      *                   DB2PRD.FLEXZ_GRP_PSTAL        GFCTB055       *
      *                   DB2PRD.FLEXZ_GRP_PAB          GFCTB054       *
      *                   DB2PRD.FLEXZ_GRP_SGMTO        GFCTB056       *
      *                   DB2PRD.FLEXZ_GRP_DEPDC        GFCTB051       *
      *                   DB2PRD.FLEXZ_GRP_CLI          GFCTB050       *
0310  *                   DB2PRD.TFLEXZ_GRP_RZ          GFCTB0M8       *
      *                   DB2PRD.FLEXZ_AGPTO_CTA        GFCTB049       *
0212  *                   DB2PRD.PARM_SERVC_TARIF       GFCTB0A2       *
110510*                   DB2PRD.RAZAO_CONTABIL         PLCCB004       *
      *                                                                *
      *----------------------------------------------------------------*
      *    BOOK'S......:                                               *
      *    I#GFCTNH - AREA DE COMUNICACAO - ENTRADA                    *
      *    I#GFCTNI - AREA DE COMUNICACAO - SAIDA                      *
      *    I#GFCTFZ - ENTRADA - VERIFICAR DISPONIBILIDADE DO ON-LINE.  *
      *    I#GFCTG1 - SAIDA   - VERIFICAR DISPONIBILIDADE DO ON-LINE.  *
      *    I#GFCTG2 - ENTRADA - OBTER DESCRICAO DE MENSAGEM.           *
      *    I#GFCTG3 - SAIDA   - OBTER DESCRICAO DE MENSAGEM.           *
      *    I#GFCTGB - AREA DE COMUNICACAO - ENTRADA                    *
      *    I#GFCTGC - AREA DE COMUNICACAO - SAIDA                      *
      *    I#GFCT1V - AREA DE COMUNICACAO - ENTRADA                    *
      *    I#GFCT1X - AREA DE COMUNICACAO - SAIDA                      *
      *    I#RURC88 - AREA PARA MODULO RURC9020                        *
      *    I#GFCT0M - AREA DE COMUNICACAO - ERROS.                     *
      *                                                                *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    GFCT5523 - OBTER DESCRICAO DE MENSAGENS.                    *
      *    GFCT5522 - VERIFICAR DISPONIBILIDADE DO ON-LINE.            *
      *    POOL0081 - ALOCAR DINAMICAMENTE UM MODULO.                  *
      *    GFCT5006 - DESCRICAO DEPENDENCIA                            *
      *    GFCT5109 - OBTER DESCRICAO DE PAB                           *
      *    DCIT8000 - OBTEM UF                                         *
      *    RURC9020 - OBTEM MUNICIPIO                                  *
      *    GFCT5090 - DESCRICAO POSTO SERVICO                          *
      *                                                                *
      *----------------------------------------------------------------*
      *    ULTIMA ALTERACAO EM 11/06/2008 - VALERIA                    *
      *    ALTERACOES REFERENTES AOS AGRUPAMENTOS PERSONALIZADOS       *
      *    - INCLUSAO DOS AGRUPAMENTOS PERSONALIZADOS                  *
      *                                                                *
      *----------------------------------------------------------------*
0212  *    ULTIMA ALTERACAO EM 02/12/2009 - HELIO SANTONI              *
0212  *    ALTERACOES NA LISTA DE FLEXIBILIZACOES DA CONSULTA VIGENTES *
0212  *    - TRATAMENTO DA COMBO CONJUNTO DE SERVICOS                  *
0212  *    - INCLUIR TRATAMENTO DA TABELA GFCTB0A2                     *
      *                                                                *
      *----------------------------------------------------------------*
0310  *    ULTIMA ALTERACAO EM 29/03/2010 - HELIO SANTONI              *
0310  *    ALTERACOES DO AGRUPAMENTO :                                 *
0310  *    - INCLUIR AGRUPAMENTO 20 (BRADESCO EXPRESSO)                *
0310  *    - INCLUIR AGRUPAMENTO 26 (RAZAO)                            *
      *                                                                *
BI0810*----------------------------------------------------------------*
BI0810*    ULTIMA ALTERACAO EM 12/08/2010 - BIRA                       *
BI0810*    FLEXIBILIZACAO PARA AGRUPAMENTO - CONSULTA VIGENTES         *
BI0810*    - ACERTO NO (DECLARE) TAB. GFCTB049 PARA NAO DUPLICAR REGS. *
BI0810*                                                                *
BI1211*----------------------------------------------------------------*
BI1211*    ULTIMA ALTERACAO EM 15/12/2011 - BIRA                       *
BI1211*    FLEXIBILIZACAO PARA AGRUPAMENTO - CONSULTA VIGENTES         *
BI1211*    - AUMENTAR O TAMANHO DOS INDEXADORES PARA SUPORTAR VOLUME   *
BI1211*      MAIOR QUE 999 OCORRENCIAS (PGM EM LOOP).                  *
BI1211*                                                                *
BI1211*    - CORRECAO NO RESTART PARA NAO DUPLICAR REGISTROS           *
BI1211*      CONSULTADOS.                                              *
BI1211*                                                                *
      *================================================================*

      *================================================================*
       ENVIRONMENT                     DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *----------------------------------------------------------------*

       SPECIAL-NAMES.
           DECIMAL-POINT               IS COMMA.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* INICIO DA WORKING STORAGE SECTION *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA INDEXADORES *'.
      *----------------------------------------------------------------*

BI1211 77  IND-1                       PIC  9(009) COMP-3  VALUE ZEROS.
BI1211 77  IND-2                       PIC  9(009) COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA VARIAVEIS AUXILIARES *'.
      *----------------------------------------------------------------*

0212   01  WRK-DINIC-FLEXZ             PIC  X(010)         VALUE SPACES.
0212   01  WRK-DFIM-FLEXZ              PIC  X(010)         VALUE SPACES.

0212   01  WRK-CDEPDC-ENCRR            PIC S9(005) COMP-3  VALUE ZEROS.
0212   01  WRK-CFUNC-ENCRR             PIC S9(009) COMP-3  VALUE ZEROS.

BI1211 01  WRK-QTDE-OCOR               PIC  9(007) COMP-3  VALUE ZEROS.
0212   01  WRK-QTDE-TOT-REG            PIC  9(009) COMP-3  VALUE ZEROS.

0212   01  WRK-CCJTO-SERVC-INI         PIC S9(003) COMP-3  VALUE ZEROS.
0212   01  WRK-CCJTO-SERVC-FIM         PIC S9(003) COMP-3  VALUE ZEROS.

0212   01  WRK-CSERVC-TARIF-S          PIC +9(005)        VALUE ZEROS.
0212   01  FILLER                      REDEFINES     WRK-CSERVC-TARIF-S.
0212       05  FILLER                  PIC  X(001).
0212       05  WRK-CSERVC-TARIF        PIC  9(005).

ST2506*01  WRK-CCGC-CPF-S              PIC +9(009)        VALUE ZEROS.
ST2506*01  FILLER                      REDEFINES       WRK-CCGC-CPF-S.
ST2506*    05  FILLER                  PIC  X(001).
ST2506*    05  WRK-CCGC-CPF            PIC  9(009).
ST2506 01  WRK-CCGC-CPF                PIC  X(009)        VALUE SPACES.

ST2506*01  WRK-CFLIAL-CGC-S            PIC +9(005)        VALUE ZEROS.
ST2506*01  FILLER                      REDEFINES       WRK-CFLIAL-CGC-S.
ST2506*    05  FILLER                  PIC  X(001).
ST2506*    05  WRK-CFLIAL-CGC          PIC  9(005).
ST2506 01  WRK-CFLIAL-CGC              PIC  X(004)        VALUE SPACES.

0212   01  WRK-CCTRL-CPF-CGC-S         PIC +9(002)        VALUE ZEROS.
0212   01  FILLER                      REDEFINES WRK-CCTRL-CPF-CGC-S.
0212       05  FILLER                  PIC  X(001).
0212       05  WRK-CCTRL-CPF-CGC       PIC  9(002).

0212   01  WRK-CDEPDC-S                PIC +9(005)        VALUE ZEROS.
0212   01  FILLER                      REDEFINES WRK-CDEPDC-S.
0212       05  FILLER                  PIC  X(001).
0212       05  WRK-CDEPDC              PIC  9(005).

0212   01  WRK-CPOSTO-SERVC-S          PIC +9(005)        VALUE ZEROS.
0212   01  FILLER                      REDEFINES WRK-CPOSTO-SERVC-S.
0212       05  FILLER                  PIC  X(001).
0212       05  WRK-CPOSTO-SERVC        PIC  9(005).

0212   01  WRK-CSGMTO-TARIF-S          PIC +9(003)        VALUE ZEROS.
0212   01  FILLER                      REDEFINES WRK-CSGMTO-TARIF-S.
0212       05  FILLER                  PIC  X(001).
0212       05  WRK-CSGMTO-TARIF        PIC  9(003).

0212   01  WRK-CMUN-IBGE-S             PIC +9(007)        VALUE ZEROS.
0212   01  FILLER                      REDEFINES WRK-CMUN-IBGE-S.
0212       05  FILLER                  PIC  X(001).
0212       05  WRK-CMUN-IBGE           PIC  9(007).

0310   01  WRK-CGRP-RZ-S               PIC +9(003)        VALUE ZEROS.
0310   01  FILLER                      REDEFINES WRK-CGRP-RZ-S.
0310       05  FILLER                  PIC  X(001).
0310       05  WRK-CGRP-RZ             PIC  9(003).

0310   01  WRK-CSGRP-RZ-S              PIC +9(003)        VALUE ZEROS.
0310   01  FILLER                      REDEFINES WRK-CSGRP-RZ-S.
0310       05  FILLER                  PIC  X(001).
0310       05  WRK-CSGRP-RZ            PIC  9(003).

HEXA   01  WRK-COD-SQL-ERRO            PIC S9(09)     VALUE ZEROS.
       01  FILLER                      REDEFINES      WRK-COD-SQL-ERRO.
           05  FILLER                  PIC  X(006).
           05  WRK-COD-SQL-ERRO-2      PIC  9(003).

       77  WRK-MODULO                  PIC  X(008)         VALUE SPACES.
       77  WRK-NOM-TAB                 PIC  X(008)         VALUE SPACES.
       77  WRK-FINALIZAR               PIC  X(001)         VALUE 'N'.
BI1211 77  WRK-DESPREZA                PIC  9(009)         VALUE ZEROS.
BI1211 77  WRK-DESPREZA-PK             PIC  9(009) COMP-3  VALUE ZEROS.
       01  WRK-POOL7100                PIC  X(107)         VALUE SPACES.
       01  WRK-SQLCA                   PIC  X(136)         VALUE SPACES.
       01  WRK-TARIFA-INI              PIC S9(005) COMP-3  VALUE ZEROS.
       01  WRK-TARIFA-FIM              PIC S9(005) COMP-3  VALUE ZEROS.
       01  WRK-COUNT                   PIC S9(009) COMP    VALUE ZEROS.

       01  WRK-MSG001.
           05 FILLER                   PIC  X(026)         VALUE
              'ERRO NA CHAMADA AO MODULO '.
           05 WRK-MODULO-MSG           PIC  X(008)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(051)         VALUE
           '* AREA DE COMUNICACAO COM DCIT8000 *'.
      *----------------------------------------------------------------*

       01  WRK-DCIT-UNIDADE-FEDERATIVA.
           05  WRK-AREA-DCLGEN-DCITV002 PIC X(070)         VALUE SPACES.
           05  WRK-DCIT-CODIGO-RETORNO  PIC 9(002)         VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(051)         VALUE
           '* AREA PARA INCLUDES  *'.
      *----------------------------------------------------------------*

       COPY 'I#GFCTFZ'.

       COPY 'I#GFCTG1'.

       COPY 'I#GFCTG2'.

       COPY 'I#GFCTG3'.

       COPY 'I#RURC88'.

       COPY 'I#GFCTGB'.

       COPY 'I#GFCTGC'.

       COPY 'I#GFCT1V'.

       COPY 'I#GFCT1X'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA TABELA DB2 *'.
      *----------------------------------------------------------------*

           EXEC SQL
             INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
             INCLUDE DCITV002
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0H5
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0A1
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0D8
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB057
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB053
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB055
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB054
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB056
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB051
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB050
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB049
           END-EXEC.

0212       EXEC SQL
0212         INCLUDE GFCTB0A2
0212       END-EXEC.

0212       EXEC SQL
0212         INCLUDE GFCTB0M8
0212       END-EXEC.

0212       EXEC SQL
0212         INCLUDE PLCCB004
0212       END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA DECLARACAO DE CURSORES *'.
      *----------------------------------------------------------------*

           EXEC SQL
             DECLARE   CSR01-GFCTB049   CURSOR   FOR
BI0810        SELECT DISTINCT
0710XS             T1.CSERVC_TARIF,
0710XS             T1.CAGPTO_CTA,
0710XS             T1.HIDTFD_FLEXZ_AGPTO,
0710XS             T1.DINIC_FLEXZ_AGPTO,
0710XS             T1.DFIM_FLEXZ_AGPTO
0212         FROM  DB2PRD.FLEXZ_AGPTO_CTA   T1,
0212               DB2PRD.PARM_SERVC_TARIF  T2
0212         WHERE
0212              (T1.CAGPTO_CTA       = :GFCTB049.CAGPTO-CTA
0212           AND T1.CDEPDC_ENCRR_VGCIA
0212                                   = :WRK-CDEPDC-ENCRR
0212           AND T1.CFUNC_ENCRR_VGCIA
0212                                   = :WRK-CFUNC-ENCRR
0212           AND (T1.DINIC_FLEXZ_AGPTO
0212                                  <= :WRK-DINIC-FLEXZ
0212           AND  T1.DFIM_FLEXZ_AGPTO
0212                                  >= :WRK-DFIM-FLEXZ ))
0212           AND T2.CSERVC_TARIF     = T1.CSERVC_TARIF
0212           AND T2.CCJTO_SERVC
0212                             BETWEEN :WRK-CCJTO-SERVC-INI
0212                                 AND :WRK-CCJTO-SERVC-FIM
             ORDER BY
                   T1.CSERVC_TARIF
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* FIM DA WORKING STORAGE SECTION *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       COPY 'I#GFCTNH'.

       COPY 'I#GFCTNI'.

       COPY 'I#GFCT0M'.

      *================================================================*
       PROCEDURE                       DIVISION USING GFCTNH-ENTRADA
                                                      GFCTNI-SAIDA
                                                      GFCT0M-AREA-ERROS.
      *================================================================*

      ******************************************************************
      *CHAMA AS ROTINAS PRINCIPAIS DO PROCESSAMENTO                    *
      ******************************************************************
      *----------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *----------------------------------------------------------------*

           PERFORM 1000-INICIALIZAR.

           PERFORM 2000-PROCESSAR.

           PERFORM 3000-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *PROCEDIMENTOS INICIAIS DO PROGRAMA                              *
      ******************************************************************
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCT0M-AREA-ERROS
                                          GFCTNI-SAIDA
           MOVE 'N'                    TO WRK-FINALIZAR

           INITIALIZE GFCTNI-SAIDA
                      GFCT0M-AREA-ERROS
                      IND-1
                      IND-2.

           PERFORM 1100-VERIFICAR-DISP-SISTEMA.

           PERFORM 1200-MONTAR-AREA-SAIDA.

           PERFORM 1300-CONSISTIR-DADOS.

           PERFORM 1400-OBTER-DESC-MSG.

           MOVE  GFCTG3-DESC-MSG       TO GFCTNI-DESC-MSG-ERRO.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *VERIFICA DISPONIBILIDADE DO SISTEMA                             *
      ******************************************************************
      *----------------------------------------------------------------*
       1100-VERIFICAR-DISP-SISTEMA     SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                     TO GFCTG1-SAIDA.

           INITIALIZE GFCTG1-SAIDA.

           MOVE +100                       TO GFCTFZ-LL.
           MOVE 'GFCT'                     TO GFCTFZ-SISTEMA.
           MOVE GFCTNH-TRANSACAO           TO GFCTFZ-TRANSACAO.
           MOVE GFCTNH-FUNCAO              TO GFCTFZ-FUNCAO.
           MOVE '0000000'                  TO GFCTFZ-FUNC-BDSCO.
           MOVE 'N'                        TO GFCTFZ-FIM.
           MOVE 'GFCT5522'                 TO WRK-MODULO.

           CALL WRK-MODULO USING
                                                 GFCTFZ-ENTRADA
                                                 GFCTG1-SAIDA
                                                 GFCT0M-AREA-ERROS.

           IF  RETURN-CODE                 NOT EQUAL ZEROS
               MOVE 9                      TO GFCTNI-ERRO
               MOVE WRK-MODULO             TO GFCT0M-PROGRAMA
                                              WRK-MODULO-MSG
               MOVE WRK-MSG001             TO GFCT0M-TEXTO
               MOVE 'APL'                  TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0422'             TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG1-ERRO                 EQUAL 9
               MOVE 'GFCT0422'             TO GFCT0M-TRANSACAO
               MOVE GFCTG1-COD-SQL-ERRO    TO GFCT0M-SQLCODE
               MOVE GFCTG1-ERRO            TO GFCTNI-COD-SQL-ERRO
               MOVE GFCTG1-COD-MSG-ERRO    TO GFCTNI-COD-MSG-ERRO
               MOVE GFCTG1-DESC-MSG-ERRO   TO GFCTNI-DESC-MSG-ERRO
               MOVE GFCTG1-DESC-MSG-ERRO(1:75) TO
                                              GFCT0M-TEXTO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG1-CSIT-ONLINE          EQUAL 1
               MOVE 9                      TO GFCTNI-ERRO
               MOVE 'SISTEMA INDISPONIVEL' TO GFCT0M-TEXTO
               MOVE 'APL'                  TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0422'             TO GFCT0M-TRANSACAO
               MOVE WRK-MODULO             TO GFCT0M-PROGRAMA
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *FORMATA AREA DE RETORNO AO CHAMADOR                             *
      ******************************************************************
      *----------------------------------------------------------------*
       1200-MONTAR-AREA-SAIDA          SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                     TO GFCTNI-SAIDA
                                              GFCT0M-AREA-ERROS.

           INITIALIZE GFCTNI-SAIDA
                      GFCT0M-AREA-ERROS.

120510     MOVE 298                        TO GFCTNI-LL.
           MOVE ZEROS                      TO GFCTNI-ZZ.
           MOVE GFCTNH-TRANSACAO           TO GFCTNI-TRANSACAO.
           MOVE GFCTNH-FUNC-BDSCO          TO GFCTNI-FUNC-BDSCO.
           MOVE GFCTNH-FUNCAO              TO GFCTNI-FUNCAO.

           IF  GFCTNH-QTDE-OCOR            NOT NUMERIC
               MOVE ZEROS                  TO GFCTNI-QTDE-OCOR
PERF                                          WRK-QTDE-OCOR
           ELSE
               MOVE GFCTNH-QTDE-OCOR       TO GFCTNI-QTDE-OCOR
PERF                                          WRK-QTDE-OCOR
           END-IF.

BI1211     IF  GFCTNH-QTDE-TOT-REG         NOT NUMERIC
BI1211         MOVE ZEROS                  TO GFCTNI-QTDE-TOT-REG
BI1211     ELSE
BI1211         MOVE GFCTNH-QTDE-TOT-REG    TO GFCTNI-QTDE-TOT-REG
BI1211     END-IF.

           MOVE 'N'                        TO GFCTNI-FIM.
           MOVE ZEROS                      TO GFCTNI-ERRO
                                              GFCTNI-COD-SQL-ERRO
                                              GFCTNI-COD-MSG-ERRO
           MOVE '0000'                     TO GFCTG2-COD-MSG.
           MOVE GFCTNH-FILTRO              TO GFCTNI-FILTRO.

0212       PERFORM
0212       VARYING IND-1               FROM 1 BY 1
120510       UNTIL IND-1               GREATER 4
ST2506*            MOVE ZEROS          TO GFCTNI-CGC-CPF     (IND-1)
ST2506*                                   GFCTNI-FILIAL      (IND-1)
0212               MOVE ZEROS          TO GFCTNI-AGENCIA     (IND-1)
0212                                      GFCTNI-POSTO       (IND-1)
0212                                      GFCTNI-SEGMENTO    (IND-1)
0212                                      GFCTNI-MUNICIPIO   (IND-1)
0212                                      GFCTNI-COD-TARIFA  (IND-1)
0212                                      GFCTNI-CONTROLE    (IND-1)
0310                                      GFCTNI-GRUPO-CTBIL (IND-1)
0310                                      GFCTNI-SUB-GRUPO-CTBIL
0310                                                         (IND-1)
ST2506             MOVE SPACES         TO GFCTNI-CGC-CPF     (IND-1)
ST2506                                    GFCTNI-FILIAL      (IND-1)
0212                                      GFCTNI-UF          (IND-1)
0212                                      GFCTNI-DESCR-AGPTO (IND-1)
0212                                      GFCTNI-DESCR-TARIFA(IND-1)
0212                                      GFCTNI-TIMESTAMP   (IND-1)
0212                                      GFCTNI-DATA-INIC   (IND-1)
0212                                      GFCTNI-DATA-FIM    (IND-1)
120510                                    GFCTNI-DESC-CTBIL  (IND-1)
0212       END-PERFORM.

           MOVE  ZEROS                 TO   IND-1.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *CONSISTE DADOS                                                  *
      ******************************************************************
      *----------------------------------------------------------------*
       1300-CONSISTIR-DADOS            SECTION.
      *----------------------------------------------------------------*

           IF (GFCTNH-TRANSACAO        EQUAL SPACES OR LOW-VALUES) OR
              (GFCTNH-FUNCAO           EQUAL SPACES OR LOW-VALUES) OR
              (GFCTNH-FUNC-BDSCO       EQUAL SPACES OR LOW-VALUES) OR
0212          (GFCTNH-FLT-CAGPTO-CTA   NOT NUMERIC               ) OR
ST2506*       (GFCTNH-FLT-CCGC-CPF     NOT NUMERIC               ) OR
ST2506*       (GFCTNH-FLT-CFLIAL-CGC   NOT NUMERIC               ) OR
ST2506        (GFCTNH-FLT-CCGC-CPF     EQUAL LOW-VALUES          ) OR
ST2506        (GFCTNH-FLT-CFLIAL-CGC   EQUAL LOW-VALUES          ) OR
0212          (GFCTNH-FLT-CCTRL-CPF-CGC NOT NUMERIC              ) OR
0212          (GFCTNH-FLT-CDEPDC       NOT NUMERIC               ) OR
0212          (GFCTNH-FLT-CPOSTO-SERVC NOT NUMERIC               ) OR
0212          (GFCTNH-FLT-CSGMTO-TARIF NOT NUMERIC               ) OR
0212          (GFCTNH-FLT-CMUN-IBGE    NOT NUMERIC               ) OR
0212          (GFCTNH-FLT-CCJTO-SERVC  NOT NUMERIC               ) OR
0310          (GFCTNI-FLT-GRUPO-CTBIL  NOT NUMERIC               ) OR
0310          (GFCTNI-FLT-SUB-GRUPO-CTBIL
0310                                   NOT NUMERIC               )
               MOVE  1                 TO GFCTNI-ERRO
               MOVE  ZEROS             TO GFCTNI-COD-SQL-ERRO
               MOVE  0001              TO GFCTNI-COD-MSG-ERRO
               MOVE '0001'             TO GFCTG2-COD-MSG
               MOVE 'S'                TO GFCTNI-FIM
               PERFORM 1400-OBTER-DESC-MSG
               MOVE  GFCTG3-DESC-MSG   TO GFCTNI-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ACESSA MODULO GFCT5523 PARA OBTER A DESCRICAO DA MENSAGEM       *
      ******************************************************************
      *----------------------------------------------------------------*
       1400-OBTER-DESC-MSG             SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                     TO GFCTG3-SAIDA.

           INITIALIZE GFCTG3-SAIDA.

           MOVE +100                       TO GFCTG2-LL.
           MOVE ZEROS                      TO GFCTG2-ZZ.
           MOVE GFCTNH-TRANSACAO           TO GFCTG2-TRANSACAO.
           MOVE GFCTNH-FUNCAO              TO GFCTG2-FUNCAO.
           MOVE GFCTNH-FUNC-BDSCO          TO GFCTG2-FUNC-BDSCO.
           MOVE 'N'                        TO GFCTG2-FIM.
           MOVE 'O'                        TO GFCTG2-TIPO-PROC.
           MOVE 'GFCT5523'                 TO WRK-MODULO.

           CALL WRK-MODULO USING
                                                 GFCTG2-ENTRADA
                                                 GFCTG3-SAIDA
                                                 GFCT0M-AREA-ERROS.

           IF  RETURN-CODE                 NOT EQUAL ZEROS
               MOVE 9                      TO GFCTNI-ERRO
               MOVE WRK-MODULO             TO GFCT0M-PROGRAMA
                                              WRK-MODULO-MSG
               MOVE WRK-MSG001             TO GFCT0M-TEXTO
               MOVE 'APL'                  TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0422'             TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG3-ERRO                 EQUAL 9
               MOVE 9                      TO GFCTNI-ERRO
               MOVE WRK-MODULO             TO GFCT0M-PROGRAMA
                                              WRK-MODULO-MSG
               MOVE WRK-MSG001             TO GFCT0M-TEXTO
               MOVE 'APL'                  TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0422'             TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG3-ERRO                 EQUAL 1
               MOVE 1                      TO GFCTNI-ERRO
               MOVE GFCTG3-COD-SQL-ERRO    TO GFCTNI-COD-SQL-ERRO
               MOVE GFCTG3-COD-MSG-ERRO    TO GFCTNI-COD-MSG-ERRO
               MOVE GFCTG3-DESC-MSG-ERRO   TO GFCTNI-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *PROCESSAMENTO PRINCIPAL                                         *
      ******************************************************************
      *----------------------------------------------------------------*
       2000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 2100-OBTER-DATA-PROCM.

ST2506*    1F  GFCTNH-FLT-CCGC-CPF      EQUAL  ZEROS      AND
ST2506     IF  GFCTNH-FLT-CCGC-CPF      EQUAL  SPACES     AND
0212           GFCTNH-FLT-CCTRL-CPF-CGC EQUAL  ZEROS      AND
0212           GFCTNH-FLT-CDEPDC        EQUAL  ZEROS      AND
0212           GFCTNH-FLT-CPOSTO-SERVC  EQUAL  ZEROS      AND
0212           GFCTNH-FLT-CSGMTO-TARIF  EQUAL  ZEROS      AND
0212           GFCTNH-FLT-CMUN-IBGE     EQUAL  ZEROS      AND
0212           GFCTNH-FLT-CCJTO-SERVC   EQUAL  ZEROS      AND
0212           GFCTNH-FLT-CSGL-UF       EQUAL  SPACES     AND
0310           GFCTNH-FLT-GRUPO-CTBIL   EQUAL  ZEROS
               PERFORM 2200-OBTER-QTDE-OCORRS
           END-IF.

           PERFORM 2300-ABRIR-CSR-GFCTB049.

           MOVE GFCTNH-QTDE-OCOR TO WRK-DESPREZA-PK.

           ADD 1 TO WRK-DESPREZA-PK.

           MOVE WRK-DESPREZA-PK TO WRK-DESPREZA.

______*____COMPUTE_WRK-DESPREZA__=_GFCTNH-QTDE-OCOR_+_1.

           PERFORM 2400-LER-CSR-GFCTB049   VARYING
                   IND-2 FROM 1 BY 1       UNTIL
                   IND-2                   GREATER WRK-DESPREZA OR
                   WRK-FINALIZAR           EQUAL 'S'.

           PERFORM 2500-PROCESSAR-CURSOR   UNTIL
120510             IND-1                   GREATER 4 OR
                   WRK-FINALIZAR           EQUAL 'S'.

           PERFORM 2600-FECHAR-CSR-GFCTB049.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *EFETUA CONSULTA NA TABELA GFCTB0A1                              *
      ******************************************************************
      *----------------------------------------------------------------*
       2100-OBTER-DATA-PROCM           SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT'                 TO CSIST-PRINC      OF GFCTB0A1.

           EXEC SQL
             SELECT
                   DPROCM_ATUAL
             INTO
                   :GFCTB0A1.DPROCM-ATUAL
             FROM   DB2PRD.PARM_DATA_PROCM
             WHERE
                   CSIST_PRINC        = :GFCTB0A1.CSIST-PRINC
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'PARM_DATA_PROCM ' TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE  0010              TO GFCTNI-COD-MSG-ERRO
               MOVE '0010'             TO GFCTG2-COD-MSG
                                          GFCT0M-LOCAL
               MOVE 'GFCTB0A1'         TO WRK-NOM-TAB
               PERFORM 2110-FORMATAR-ERRO-DB2
           END-IF.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *FORMATA AREA DE ERRO EM ACESSO A TABELAS                        *
      ******************************************************************
      *----------------------------------------------------------------*
       2110-FORMATAR-ERRO-DB2          SECTION.
      *----------------------------------------------------------------*

           MOVE  9                     TO GFCTNI-ERRO.
           MOVE 'GFCT0422'             TO GFCT0M-PROGRAMA.
           MOVE 'DB2'                  TO GFCT0M-TIPO-ACESSO.
           MOVE  SQLCA                 TO GFCT0M-SQLCA-AREA.
           MOVE  SQLCODE               TO GFCT0M-SQLCODE
HEXA                                      WRK-COD-SQL-ERRO
HEXA       MOVE WRK-COD-SQL-ERRO-2   TO   GFCTNI-COD-SQL-ERRO.

           PERFORM 1400-OBTER-DESC-MSG.

           STRING GFCTG3-DESC-MSG ' - '   WRK-NOM-TAB
           DELIMITED BY '  '         INTO GFCTNI-DESC-MSG-ERRO.

           PERFORM 3000-FINALIZAR.

      *----------------------------------------------------------------*
       2110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *EFETUA SELECT COUNT NA TABELA GFCTB049                          *
      ******************************************************************
      *----------------------------------------------------------------*
       2200-OBTER-QTDE-OCORRS          SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTNH-FLT-CAGPTO-CTA  TO CAGPTO-CTA         OF GFCTB049
           MOVE DPROCM-ATUAL           OF GFCTB0A1
                                       TO WRK-DINIC-FLEXZ
                                          WRK-DFIM-FLEXZ
           MOVE ZEROS                  TO CDEPDC-ENCRR-VGCIA OF GFCTB049
                                          CFUNC-ENCRR-VGCIA  OF GFCTB049

           IF  GFCTNH-FLT-CCJTO-SERVC  EQUAL ZEROS
               MOVE ZEROS              TO WRK-CCJTO-SERVC-INI
               MOVE 99999              TO WRK-CCJTO-SERVC-FIM
           ELSE
               MOVE GFCTNH-FLT-CCJTO-SERVC
                                       TO WRK-CCJTO-SERVC-INI
                                          WRK-CCJTO-SERVC-FIM
           END-IF.

           EXEC SQL
             SELECT  COUNT(*)
               INTO   :WRK-COUNT
               FROM  DB2PRD.FLEXZ_AGPTO_CTA
             WHERE
                   (CAGPTO_CTA          = :GFCTB049.CAGPTO-CTA
               AND  CDEPDC_ENCRR_VGCIA  = :GFCTB049.CDEPDC-ENCRR-VGCIA
               AND  CFUNC_ENCRR_VGCIA   = :GFCTB049.CFUNC-ENCRR-VGCIA
               AND (DINIC_FLEXZ_AGPTO  <= :WRK-DINIC-FLEXZ
               AND  DFIM_FLEXZ_AGPTO   >= :WRK-DFIM-FLEXZ))
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'FLEXZ_AGPTO_CTA ' TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE  0010              TO GFCTNI-COD-MSG-ERRO
               MOVE '0010'             TO GFCTG2-COD-MSG
               MOVE '0020'             TO GFCT0M-LOCAL
               MOVE 'GFCTB049'         TO WRK-NOM-TAB
               PERFORM 2110-FORMATAR-ERRO-DB2
           END-IF.

           IF  WRK-COUNT               GREATER 150
               MOVE  1                 TO GFCTNI-ERRO
               MOVE  ZEROS             TO GFCTNI-COD-SQL-ERRO
               MOVE  0633              TO GFCTNI-COD-MSG-ERRO
               MOVE '0633'             TO GFCTG2-COD-MSG
               MOVE 'S'                TO GFCTNI-FIM
               PERFORM 1400-OBTER-DESC-MSG
               MOVE  GFCTG3-DESC-MSG   TO GFCTNI-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ABRE CURSOR DA TABELA GFCTB049                                  *
      ******************************************************************
      *----------------------------------------------------------------*
       2300-ABRIR-CSR-GFCTB049         SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTNH-FLT-CAGPTO-CTA  TO CAGPTO-CTA         OF GFCTB049
           MOVE DPROCM-ATUAL           OF GFCTB0A1
                                       TO WRK-DINIC-FLEXZ
                                          WRK-DFIM-FLEXZ
           MOVE ZEROS                  TO WRK-CDEPDC-ENCRR
                                          WRK-CFUNC-ENCRR

           IF  GFCTNH-FLT-CCJTO-SERVC  EQUAL ZEROS
               MOVE ZEROS              TO WRK-CCJTO-SERVC-INI
               MOVE 99999              TO WRK-CCJTO-SERVC-FIM
           ELSE
               MOVE GFCTNH-FLT-CCJTO-SERVC
                                       TO WRK-CCJTO-SERVC-INI
                                          WRK-CCJTO-SERVC-FIM
           END-IF.

           EXEC SQL
               OPEN CSR01-GFCTB049
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'FLEXZ_AGPTO_CTA ' TO GFCT0M-NOME-TAB
               MOVE 'GFCTB049'         TO WRK-NOM-TAB
               MOVE 'OPEN'             TO GFCT0M-COMANDO-SQL
               MOVE '0030'             TO GFCT0M-LOCAL
               MOVE  0010              TO GFCTNI-COD-MSG-ERRO
               MOVE '0010'             TO GFCTG2-COD-MSG
               PERFORM 2110-FORMATAR-ERRO-DB2
           END-IF.

      *----------------------------------------------------------------*
       2300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *EFETUA LEITURA NO CURSOR DA TABELA GFCTB049                     *
      ******************************************************************
      *----------------------------------------------------------------*
       2400-LER-CSR-GFCTB049           SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
0710XS        FETCH  CSR01-GFCTB049
0710XS         INTO
0710XS                :GFCTB049.CSERVC-TARIF,
0710XS                :GFCTB049.CAGPTO-CTA,
0710XS                :GFCTB049.HIDTFD-FLEXZ-AGPTO,
0710XS                :GFCTB049.DINIC-FLEXZ-AGPTO,
0710XS                :GFCTB049.DFIM-FLEXZ-AGPTO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'FLEXZ_AGPTO_CTA ' TO GFCT0M-NOME-TAB
               MOVE 'GFCTB049'         TO WRK-NOM-TAB
               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE '0040'             TO GFCT0M-LOCAL
               MOVE  0006              TO GFCTNI-COD-MSG-ERRO
               MOVE '0006'             TO GFCTG2-COD-MSG
               PERFORM 2110-FORMATAR-ERRO-DB2
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FINALIZAR
                                          GFCTNI-FIM
               IF  IND-1               EQUAL ZEROS
                   MOVE ZEROS          TO GFCTNI-ERRO
                                          GFCTNI-COD-SQL-ERRO
                   MOVE 0007           TO GFCTNI-COD-MSG-ERRO
                   MOVE '0007'         TO GFCTG2-COD-MSG
                   PERFORM 1400-OBTER-DESC-MSG
                   STRING GFCTG3-DESC-MSG ' - ' WRK-NOM-TAB
                   DELIMITED BY '  '   INTO GFCTNI-DESC-MSG-ERRO
BI1211             PERFORM 3000-FINALIZAR
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *TRATAR CURSORES                                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       2500-PROCESSAR-CURSOR           SECTION.
      *----------------------------------------------------------------*

           EVALUATE GFCTNH-FLT-CAGPTO-CTA

               WHEN 15
                   PERFORM 2510-SELECIONAR-PAB
               WHEN 14
                   PERFORM 2520-SELECIONAR-AGENCIA
               WHEN 17
                   PERFORM 2530-SELECIONAR-POSTAL
               WHEN 4
                   PERFORM 2540-SELECIONAR-CLIENTE
               WHEN 9
                   PERFORM 2550-SELECIONAR-SEGMENTO
               WHEN 18
                   PERFORM 2560-SELECIONAR-MUNICIPIO
               WHEN 19
                   PERFORM 2570-SELECIONAR-UF
0310           WHEN 20
0310               PERFORM 2530-SELECIONAR-POSTAL
0310           WHEN 26
0310               PERFORM 2590-SELECIONAR-RAZAO

               WHEN OTHER
0212               PERFORM 2580-MONTAR-OCOR-SAIDA

           END-EVALUATE.

           PERFORM 2400-LER-CSR-GFCTB049.

120510     IF  IND-1                   EQUAL 4 AND
               WRK-FINALIZAR           NOT EQUAL 'S'
               MOVE 'S'                TO WRK-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ACESSA TABELA GFCTB054                                          *
      ******************************************************************
      *----------------------------------------------------------------*
       2510-SELECIONAR-PAB             SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF           OF GFCTB049
                                       TO CSERVC-TARIF      OF GFCTB054.
           MOVE CAGPTO-CTA             OF GFCTB049
                                       TO CAGPTO-CTA        OF GFCTB054.
           MOVE HIDTFD-FLEXZ-AGPTO     OF GFCTB049
                                       TO HIDTFD-FLEXZ-AGPTO
                                                            OF GFCTB054.

           EXEC SQL
             SELECT
                   CEMPR_INC,
                   CDEPDC,
                   CPOSTO_SERVC
             INTO
                   :GFCTB054.CEMPR-INC,
                   :GFCTB054.CDEPDC,
                   :GFCTB054.CPOSTO-SERVC
             FROM   DB2PRD.FLEXZ_GRP_PAB
             WHERE
                   CSERVC_TARIF       = :GFCTB054.CSERVC-TARIF       AND
                   CAGPTO_CTA         = :GFCTB054.CAGPTO-CTA         AND
                   HIDTFD_FLEXZ_AGPTO = :GFCTB054.HIDTFD-FLEXZ-AGPTO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'FLEXZ_GRP_PAB'    TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0050'             TO GFCT0M-LOCAL
               MOVE  0010              TO GFCTNI-COD-MSG-ERRO
               MOVE '0010'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB054'         TO WRK-NOM-TAB
               PERFORM 2110-FORMATAR-ERRO-DB2
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE  1                 TO GFCTNI-ERRO
               MOVE  ZEROS             TO GFCTNI-COD-SQL-ERRO
               MOVE  0926              TO GFCTNI-COD-MSG-ERRO
               MOVE '0926'             TO GFCTG2-COD-MSG
               MOVE 'S'                TO GFCTNI-FIM
               PERFORM 1400-OBTER-DESC-MSG
               MOVE  GFCTG3-DESC-MSG   TO GFCTNI-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE CDEPDC                 OF GFCTB054
                                       TO WRK-CDEPDC-S
           MOVE CPOSTO-SERVC           OF GFCTB054
                                       TO WRK-CPOSTO-SERVC-S

0212       IF (GFCTNH-FLT-CDEPDC           EQUAL ZEROS            AND
0212           GFCTNH-FLT-CPOSTO-SERVC     EQUAL ZEROS           ) OR
0212          (GFCTNH-FLT-CDEPDC           EQUAL WRK-CDEPDC       AND
0212           GFCTNH-FLT-CPOSTO-SERVC     EQUAL WRK-CPOSTO-SERVC)
0212           PERFORM 2580-MONTAR-OCOR-SAIDA
0212           MOVE  WRK-CDEPDC            TO GFCTNI-AGENCIA(IND-1)
0212           MOVE  WRK-CPOSTO-SERVC      TO GFCTNI-POSTO(IND-1)
0212           PERFORM 2511-OBTER-DESC-PAB
0212           MOVE  GFCTGC-DESC-POSTO(1)  TO GFCTNI-DESCR-AGPTO(IND-1)
BI1211     ELSE
BI1211         ADD 1                       TO WRK-QTDE-OCOR
BI1211         MOVE  WRK-QTDE-OCOR         TO GFCTNI-QTDE-OCOR
0212       END-IF.

      *----------------------------------------------------------------*
       2510-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ACESSA MODULO GFCT5109                                          *
      ******************************************************************
      *----------------------------------------------------------------*
       2511-OBTER-DESC-PAB             SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTGB-ENTRADA
                                          GFCTGC-SAIDA
                                          GFCT0M-AREA-ERROS.

           INITIALIZE GFCTGB-ENTRADA
                      GFCTGC-SAIDA
                      GFCT0M-AREA-ERROS.

           MOVE 100                    TO GFCTGB-LL.
           MOVE ZEROS                  TO GFCTGB-ZZ.
           MOVE GFCTNH-TRANSACAO       TO GFCTGB-TRANSACAO.
           MOVE GFCTNH-FUNCAO          TO GFCTGB-FUNCAO.
           MOVE ZEROS                  TO GFCTGB-QTDE-OCOR
                                          GFCTGB-QTDE-TOT-REG.
           MOVE GFCTNH-FUNC-BDSCO      TO GFCTGB-FUNC-BDSCO.
           MOVE 'N'                    TO GFCTGB-FIM.
           MOVE CDEPDC                 OF GFCTB054
                                       TO GFCTGB-COD-DEPDC.
           MOVE CPOSTO-SERVC           OF GFCTB054
                                       TO GFCTGB-COD-POSTO.
           MOVE 'GFCT5109'             TO WRK-MODULO.

           CALL WRK-MODULO USING
                                             GFCTGB-ENTRADA
                                             GFCTGC-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 9                  TO GFCTNI-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-MODULO-MSG
               MOVE WRK-MSG001         TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0422'         TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTGC-ERRO             NOT EQUAL ZEROS
               MOVE 'S'                TO GFCTNI-FIM
               MOVE GFCTGC-ERRO        TO GFCTNI-ERRO
               MOVE GFCTGC-COD-SQL-ERRO    TO GFCTNI-COD-SQL-ERRO
               MOVE GFCTGC-COD-MSG-ERRO    TO GFCTNI-COD-MSG-ERRO
               IF  GFCTGC-ERRO         EQUAL 1
                   MOVE GFCTGC-DESC-MSG-ERRO
                                       TO GFCTNI-DESC-MSG-ERRO
               ELSE
                   STRING 'GFCT5109 - ' GFCTGC-DESC-MSG-ERRO
                   DELIMITED BY SIZE   INTO GFCTNI-DESC-MSG-ERRO
               END-IF
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2511-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ACESSA TABELA GFCTB051                                          *
      ******************************************************************
      *----------------------------------------------------------------*
       2520-SELECIONAR-AGENCIA         SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF           OF GFCTB049
                                       TO CSERVC-TARIF      OF GFCTB051.
           MOVE CAGPTO-CTA             OF GFCTB049
                                       TO CAGPTO-CTA        OF GFCTB051.
           MOVE HIDTFD-FLEXZ-AGPTO     OF GFCTB049
                                       TO HIDTFD-FLEXZ-AGPTO
                                                            OF GFCTB051.

           EXEC SQL
             SELECT
                   CEMPR_INC,
                   CDEPDC
             INTO
                   :GFCTB051.CEMPR-INC,
                   :GFCTB051.CDEPDC
             FROM   DB2PRD.FLEXZ_GRP_DEPDC
             WHERE
                   CSERVC_TARIF       = :GFCTB051.CSERVC-TARIF       AND
                   CAGPTO_CTA         = :GFCTB051.CAGPTO-CTA         AND
                   HIDTFD_FLEXZ_AGPTO = :GFCTB051.HIDTFD-FLEXZ-AGPTO
           END-EXEC.


           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'FLEXZ_GRP_AG'     TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0060'             TO GFCT0M-LOCAL
               MOVE  0010              TO GFCTNI-COD-MSG-ERRO
               MOVE '0010'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB051'         TO WRK-NOM-TAB
               PERFORM 2110-FORMATAR-ERRO-DB2
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE  1                 TO GFCTNI-ERRO
               MOVE  ZEROS             TO GFCTNI-COD-SQL-ERRO
               MOVE  0926              TO GFCTNI-COD-MSG-ERRO
               MOVE '0926'             TO GFCTG2-COD-MSG
               MOVE 'S'                TO GFCTNI-FIM
               PERFORM 1400-OBTER-DESC-MSG
               MOVE  GFCTG3-DESC-MSG   TO GFCTNI-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

0212       MOVE  CDEPDC  OF GFCTB051   TO WRK-CDEPDC-S
0212
0212       IF  GFCTNH-FLT-CDEPDC          EQUAL ZEROS      OR
0212          (GFCTNH-FLT-CDEPDC          NOT EQUAL ZEROS AND
0212           GFCTNH-FLT-CDEPDC          EQUAL WRK-CDEPDC)
0212
0212           PERFORM 2580-MONTAR-OCOR-SAIDA
0212
0212           MOVE  WRK-CDEPDC        TO GFCTNI-AGENCIA(IND-1)
0212
0212           IF  CDEPDC  OF GFCTB051    EQUAL ZEROS
0212               MOVE 'TODAS'        TO GFCTNI-DESCR-AGPTO(IND-1)
0212
0212           ELSE
0212               PERFORM 2521-OBTER-DESC-AGENCIA
0212               MOVE GFCT1X-DESC-DEPDC(1)  (1:30)
0212                                   TO GFCTNI-DESCR-AGPTO(IND-1)
0212
0212           END-IF
BI1211     ELSE
BI1211         ADD 1                       TO WRK-QTDE-OCOR
BI1211         MOVE  WRK-QTDE-OCOR         TO GFCTNI-QTDE-OCOR
0212       END-IF.

      *----------------------------------------------------------------*
       2520-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ACESSA MODULO GFCT5006                                          *
      ******************************************************************
      *----------------------------------------------------------------*
       2521-OBTER-DESC-AGENCIA         SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCT1V-ENTRADA
                                          GFCT1X-SAIDA
                                          GFCT0M-AREA-ERROS.

           INITIALIZE GFCT1V-ENTRADA
                      GFCT1X-SAIDA
                      GFCT0M-AREA-ERROS.

           MOVE 660                    TO GFCT1V-LL.
           MOVE ZEROS                  TO GFCT1V-ZZ.
           MOVE GFCTNH-TRANSACAO       TO GFCT1V-TRANSACAO.
           MOVE GFCTNH-FUNCAO          TO GFCT1V-FUNCAO.
           MOVE 001                    TO GFCT1V-QTDE-OCOR.
           MOVE ZEROS                  TO GFCT1V-QTDE-TOT-REG.
           MOVE GFCTNH-FUNC-BDSCO      TO GFCT1V-FUNC-BDSCO.
           MOVE 'N'                    TO GFCT1V-FIM.
           MOVE CDEPDC                 OF GFCTB051
                                       TO GFCT1V-COD-DEPDC(1).
           MOVE 'GFCT5006'             TO WRK-MODULO.

           CALL WRK-MODULO USING
                                             GFCT1V-ENTRADA
                                             GFCT1X-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 9                  TO GFCTNI-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-MODULO-MSG
               MOVE WRK-MSG001         TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0422'         TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCT1X-ERRO             NOT EQUAL ZEROS
               MOVE 'S'                TO GFCTNI-FIM
               MOVE GFCT1X-ERRO        TO GFCTNI-ERRO
               MOVE GFCT1X-COD-SQL-ERRO    TO GFCTNI-COD-SQL-ERRO
               MOVE GFCT1X-COD-MSG-ERRO    TO GFCTNI-COD-MSG-ERRO
               IF  GFCT1X-ERRO         EQUAL 1
                   MOVE GFCT1X-DESC-MSG-ERRO
                                       TO GFCTNI-DESC-MSG-ERRO
               ELSE
                   STRING 'GFCT5006 - ' GFCT1X-DESC-MSG-ERRO
                   DELIMITED BY SIZE   INTO GFCTNI-DESC-MSG-ERRO
               END-IF
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2521-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ACESSA TABELA GFCTB055 E O MODULO GFCT5090                      *
      ******************************************************************
      *----------------------------------------------------------------*
       2530-SELECIONAR-POSTAL          SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF           OF GFCTB049
                                       TO CSERVC-TARIF      OF GFCTB055.
           MOVE CAGPTO-CTA             OF GFCTB049
                                       TO CAGPTO-CTA        OF GFCTB055.
           MOVE HIDTFD-FLEXZ-AGPTO     OF GFCTB049
                                       TO HIDTFD-FLEXZ-AGPTO
                                                            OF GFCTB055.

           EXEC SQL
             SELECT
                   CEMPR_INC,
                   CDEPDC,
                   CPOSTO_SERVC
             INTO
                   :GFCTB055.CEMPR-INC,
                   :GFCTB055.CDEPDC,
                   :GFCTB055.CPOSTO-SERVC
             FROM   DB2PRD.FLEXZ_GRP_PSTAL
             WHERE
                   CSERVC_TARIF       = :GFCTB055.CSERVC-TARIF       AND
                   CAGPTO_CTA         = :GFCTB055.CAGPTO-CTA         AND
                   HIDTFD_FLEXZ_AGPTO = :GFCTB055.HIDTFD-FLEXZ-AGPTO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'FLEXZ_GRP_PSTAL'  TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0070'             TO GFCT0M-LOCAL
               MOVE  0010              TO GFCTNI-COD-MSG-ERRO
               MOVE '0010'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB055'         TO WRK-NOM-TAB
               PERFORM 2110-FORMATAR-ERRO-DB2
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE  1                 TO GFCTNI-ERRO
               MOVE  ZEROS             TO GFCTNI-COD-SQL-ERRO
               MOVE  0926              TO GFCTNI-COD-MSG-ERRO
               MOVE '0926'             TO GFCTG2-COD-MSG
               MOVE 'S'                TO GFCTNI-FIM
               PERFORM 1400-OBTER-DESC-MSG
               MOVE  GFCTG3-DESC-MSG   TO GFCTNI-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE SPACES                 TO GFCTGB-ENTRADA
                                          GFCTGC-SAIDA
                                          GFCT0M-AREA-ERROS.

           INITIALIZE GFCTGB-ENTRADA
                      GFCTGC-SAIDA
                      GFCT0M-AREA-ERROS.

           MOVE ZEROS                  TO GFCTGB-ZZ
                                          GFCTGB-QTDE-OCOR
                                          GFCTGB-QTDE-TOT-REG.
           MOVE GFCTNH-TRANSACAO       TO GFCTGB-TRANSACAO.
           MOVE GFCTNH-FUNC-BDSCO      TO GFCTGB-FUNC-BDSCO.
           MOVE GFCTNH-FUNCAO          TO GFCTGB-FUNCAO.
           MOVE 'N'                    TO GFCTGB-FIM.
           MOVE CDEPDC                 OF GFCTB055
                                       TO GFCTGB-COD-DEPDC.
           MOVE CPOSTO-SERVC           OF GFCTB055
                                       TO GFCTGB-COD-POSTO.
           MOVE 'GFCT5090'             TO WRK-MODULO.

           CALL WRK-MODULO USING
                                             GFCTGB-ENTRADA
                                             GFCTGC-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 9                  TO GFCTNI-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-MODULO-MSG
               MOVE WRK-MSG001         TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0422'         TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTGC-ERRO             NOT EQUAL ZEROS
               MOVE 'S'                TO GFCTNI-FIM
               MOVE GFCTGC-ERRO        TO GFCTNI-ERRO
               MOVE GFCTGC-COD-SQL-ERRO    TO GFCTNI-COD-SQL-ERRO
               MOVE GFCTGC-COD-MSG-ERRO    TO GFCTNI-COD-MSG-ERRO
               IF  GFCTGC-ERRO         EQUAL 1
                   MOVE GFCTGC-DESC-MSG-ERRO
                                       TO GFCTNI-DESC-MSG-ERRO
               ELSE
                   STRING 'GFCT5090 - ' GFCTGC-DESC-MSG-ERRO
                   DELIMITED BY SIZE   INTO GFCTNI-DESC-MSG-ERRO
               END-IF
               PERFORM 3000-FINALIZAR
           END-IF.

0212       MOVE  CDEPDC       OF GFCTB055  TO WRK-CDEPDC-S
0212       MOVE  CPOSTO-SERVC OF GFCTB055  TO WRK-CPOSTO-SERVC-S
0212
0212       IF (GFCTNH-FLT-CDEPDC           EQUAL ZEROS            AND
0212           GFCTNH-FLT-CPOSTO-SERVC     EQUAL ZEROS           ) OR
0212          ((GFCTNH-FLT-CDEPDC          NOT EQUAL ZEROS        AND
0212            GFCTNH-FLT-CDEPDC          EQUAL WRK-CDEPDC)      AND
0212           (GFCTNH-FLT-CPOSTO-SERVC    NOT EQUAL ZEROS        AND
0212            GFCTNH-FLT-CPOSTO-SERVC    EQUAL WRK-CPOSTO-SERVC))
0212           PERFORM 2580-MONTAR-OCOR-SAIDA
0212           MOVE  WRK-CDEPDC            TO GFCTNI-AGENCIA(IND-1)
0212           MOVE  WRK-CPOSTO-SERVC      TO GFCTNI-POSTO(IND-1)
0212           MOVE  GFCTGC-DESC-POSTO(1)  TO GFCTNI-DESCR-AGPTO(IND-1)
BI1211     ELSE
BI1211         ADD 1                       TO WRK-QTDE-OCOR
BI1211         MOVE  WRK-QTDE-OCOR         TO GFCTNI-QTDE-OCOR
0212       END-IF.

      *----------------------------------------------------------------*
       2530-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ACESSA TABELA GFCTB050                                          *
      ******************************************************************
      *----------------------------------------------------------------*
       2540-SELECIONAR-CLIENTE         SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF           OF GFCTB049
                                       TO CSERVC-TARIF      OF GFCTB050.
           MOVE CAGPTO-CTA             OF GFCTB049
                                       TO CAGPTO-CTA        OF GFCTB050.
           MOVE HIDTFD-FLEXZ-AGPTO     OF GFCTB049
                                       TO HIDTFD-FLEXZ-AGPTO
                                                            OF GFCTB050.

           EXEC SQL
             SELECT
ST2506*            CCGC_CPF,
ST2506*            CFLIAL_CGC,
ST2506*            CCTRL_CPF_CGC
ST2506             CCGC_CPF_ST,
ST2506             CFLIAL_CGC_ST,
ST2506             CCTRL_CPF_CGC_ST
             INTO
ST2506*            :GFCTB050.CCGC-CPF,
ST2506*            :GFCTB050.CFLIAL-CGC,
ST2506*            :GFCTB050.CCTRL-CPF-CGC
ST2506             :GFCTB050.CCGC-CPF-ST,
ST2506             :GFCTB050.CFLIAL-CGC-ST,
ST2506             :GFCTB050.CCTRL-CPF-CGC-ST
             FROM   DB2PRD.FLEXZ_GRP_CLI
             WHERE
                   CSERVC_TARIF       = :GFCTB050.CSERVC-TARIF       AND
                   CAGPTO_CTA         = :GFCTB050.CAGPTO-CTA         AND
                   HIDTFD_FLEXZ_AGPTO = :GFCTB050.HIDTFD-FLEXZ-AGPTO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'FLEXZ_GRP_CLI'    TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0080'             TO GFCT0M-LOCAL
               MOVE  0010              TO GFCTNI-COD-MSG-ERRO
               MOVE '0010'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB050'         TO WRK-NOM-TAB
               PERFORM 2110-FORMATAR-ERRO-DB2
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE  1                 TO GFCTNI-ERRO
               MOVE  ZEROS             TO GFCTNI-COD-SQL-ERRO
               MOVE  0926              TO GFCTNI-COD-MSG-ERRO
               MOVE '0926'             TO GFCTG2-COD-MSG
               MOVE 'S'                TO GFCTNI-FIM
               PERFORM 1400-OBTER-DESC-MSG
               MOVE  GFCTG3-DESC-MSG   TO GFCTNI-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

ST2506*    MOVE  CCGC-CPF              OF GFCTB050
ST2506*                                TO WRK-CCGC-CPF-S
ST2506*    MOVE  CFLIAL-CGC            OF GFCTB050
ST2506*                                TO WRK-CFLIAL-CGC-S
ST2506*    MOVE  CCTRL-CPF-CGC         OF GFCTB050
ST2506*                                TO WRK-CCTRL-CPF-CGC-S
ST2506     MOVE  CCGC-CPF-ST           OF GFCTB050
ST2506                                 TO WRK-CCGC-CPF
ST2506     MOVE  CFLIAL-CGC-ST         OF GFCTB050
ST2506                                 TO WRK-CFLIAL-CGC
ST2506     MOVE  CCTRL-CPF-CGC-ST      OF GFCTB050
ST2506                                 TO WRK-CCTRL-CPF-CGC-S
0212
ST2506*    1F (GFCTNH-FLT-CCGC-CPF         EQUAL ZEROS             AND
ST2506*        GFCTNH-FLT-CFLIAL-CGC       EQUAL ZEROS             AND
ST2506     IF (GFCTNH-FLT-CCGC-CPF         EQUAL SPACES            AND
ST2506         GFCTNH-FLT-CFLIAL-CGC       EQUAL SPACES            AND
0212           GFCTNH-FLT-CCTRL-CPF-CGC    EQUAL ZEROS            )
0212           PERFORM 2580-MONTAR-OCOR-SAIDA
0212           MOVE  WRK-CCGC-CPF          TO GFCTNI-CGC-CPF(IND-1)
0212           MOVE  WRK-CFLIAL-CGC        TO GFCTNI-FILIAL(IND-1)
0212           MOVE  WRK-CCTRL-CPF-CGC     TO GFCTNI-CONTROLE(IND-1)
0212       ELSE
0212          IF (GFCTNH-FLT-CCGC-CPF     EQUAL WRK-CCGC-CPF      AND
0212              GFCTNH-FLT-CFLIAL-CGC   EQUAL WRK-CFLIAL-CGC    AND
0212              GFCTNH-FLT-CCTRL-CPF-CGC
0212                                      EQUAL WRK-CCTRL-CPF-CGC)
0212              PERFORM 2580-MONTAR-OCOR-SAIDA
0212              MOVE  WRK-CCGC-CPF          TO GFCTNI-CGC-CPF(IND-1)
0212              MOVE  WRK-CFLIAL-CGC        TO GFCTNI-FILIAL(IND-1)
0212              MOVE  WRK-CCTRL-CPF-CGC     TO GFCTNI-CONTROLE(IND-1)
BI1211        ELSE
BI1211            ADD 1                       TO WRK-QTDE-OCOR
BI1211            MOVE  WRK-QTDE-OCOR         TO GFCTNI-QTDE-OCOR
0212          END-IF
0212       END-IF.

      *----------------------------------------------------------------*
       2540-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ACESSA TABELA GFCTB056                                          *
      ******************************************************************
      *----------------------------------------------------------------*
       2550-SELECIONAR-SEGMENTO        SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF           OF GFCTB049
                                       TO CSERVC-TARIF      OF GFCTB056.
           MOVE CAGPTO-CTA             OF GFCTB049
                                       TO CAGPTO-CTA        OF GFCTB056.
           MOVE HIDTFD-FLEXZ-AGPTO     OF GFCTB049
                                       TO HIDTFD-FLEXZ-AGPTO
                                                            OF GFCTB056.

           EXEC SQL
             SELECT
                   CSGMTO_GSTAO_TARIF
             INTO
                   :GFCTB056.CSGMTO-GSTAO-TARIF
             FROM   DB2PRD.FLEXZ_GRP_SGMTO
             WHERE
                   CSERVC_TARIF       = :GFCTB056.CSERVC-TARIF       AND
                   CAGPTO_CTA         = :GFCTB056.CAGPTO-CTA         AND
                   HIDTFD_FLEXZ_AGPTO = :GFCTB056.HIDTFD-FLEXZ-AGPTO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'FLEXZ_GRP_SGMTO'  TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0090'             TO GFCT0M-LOCAL
               MOVE  0010              TO GFCTNI-COD-MSG-ERRO
               MOVE '0010'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB056'         TO WRK-NOM-TAB
               PERFORM 2110-FORMATAR-ERRO-DB2
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE  1                 TO GFCTNI-ERRO
               MOVE  ZEROS             TO GFCTNI-COD-SQL-ERRO
               MOVE  0926              TO GFCTNI-COD-MSG-ERRO
               MOVE '0926'             TO GFCTG2-COD-MSG
               MOVE 'S'                TO GFCTNI-FIM
               PERFORM 1400-OBTER-DESC-MSG
               MOVE  GFCTG3-DESC-MSG   TO GFCTNI-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

0212       MOVE  CSGMTO-GSTAO-TARIF    OF GFCTB056
0212                                   TO WRK-CSGMTO-TARIF-S
0212
0212       IF  GFCTNH-FLT-CSGMTO-TARIF    EQUAL ZEROS      OR
0212           GFCTNH-FLT-CSGMTO-TARIF    EQUAL WRK-CSGMTO-TARIF
0212           PERFORM 2580-MONTAR-OCOR-SAIDA
0212           MOVE  WRK-CSGMTO-TARIF  TO GFCTNI-SEGMENTO(IND-1)
0212           PERFORM 2551-OBTER-DESC-SEGMENTO
0212           MOVE ISGMTO-GSTAO-TARIF OF GFCTB0H5
0212                                   TO GFCTNI-DESCR-AGPTO(IND-1)
BI1211     ELSE
BI1211         ADD 1                   TO WRK-QTDE-OCOR
BI1211         MOVE  WRK-QTDE-OCOR     TO GFCTNI-QTDE-OCOR
0212       END-IF.

      *----------------------------------------------------------------*
       2550-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ACESSA TABELA GFCTB0H5                                          *
      ******************************************************************
      *----------------------------------------------------------------*
       2551-OBTER-DESC-SEGMENTO        SECTION.
      *----------------------------------------------------------------*

           MOVE CSGMTO-GSTAO-TARIF     OF GFCTB056
                                       TO CSGMTO-GSTAO-TARIF
                                       OF GFCTB0H5.

           EXEC SQL
             SELECT
                   ISGMTO_GSTAO_TARIF
             INTO
                   :GFCTB0H5.ISGMTO-GSTAO-TARIF
             FROM   DB2PRD.TTPO_SGMTO_GSTAO
             WHERE
                   CSGMTO_GSTAO_TARIF = :GFCTB0H5.CSGMTO-GSTAO-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'TTPO_SGMTO_GSTAO' TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0100'             TO GFCT0M-LOCAL
               MOVE  0010              TO GFCTNI-COD-MSG-ERRO
               MOVE '0010'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0H5'         TO WRK-NOM-TAB
               PERFORM 2110-FORMATAR-ERRO-DB2
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE  1                 TO GFCTNI-ERRO
               MOVE  ZEROS             TO GFCTNI-COD-SQL-ERRO
               MOVE  0500              TO GFCTNI-COD-MSG-ERRO
               MOVE '0500'             TO GFCTG2-COD-MSG
               MOVE 'S'                TO GFCTNI-FIM
               PERFORM 1400-OBTER-DESC-MSG
               MOVE  GFCTG3-DESC-MSG   TO GFCTNI-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2551-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ACESSA TABELA GFCTB053                                          *
      ******************************************************************
      *----------------------------------------------------------------*
       2560-SELECIONAR-MUNICIPIO       SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF           OF GFCTB049
                                       TO CSERVC-TARIF      OF GFCTB053.
           MOVE CAGPTO-CTA             OF GFCTB049
                                       TO CAGPTO-CTA        OF GFCTB053.
           MOVE HIDTFD-FLEXZ-AGPTO     OF GFCTB049
                                       TO HIDTFD-FLEXZ-AGPTO
                                                            OF GFCTB053.

           EXEC SQL
             SELECT
                   CMUN_IBGE
             INTO
                   :GFCTB053.CMUN-IBGE
             FROM   DB2PRD.FLEXZ_GRP_MUN
             WHERE
                   CSERVC_TARIF       = :GFCTB053.CSERVC-TARIF       AND
                   CAGPTO_CTA         = :GFCTB053.CAGPTO-CTA         AND
                   HIDTFD_FLEXZ_AGPTO = :GFCTB053.HIDTFD-FLEXZ-AGPTO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'FLEXZ_GRP_MUN'    TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0110'             TO GFCT0M-LOCAL
               MOVE  0010              TO GFCTNI-COD-MSG-ERRO
               MOVE '0010'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB053'         TO WRK-NOM-TAB
               PERFORM 2110-FORMATAR-ERRO-DB2
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE  1                 TO GFCTNI-ERRO
               MOVE  ZEROS             TO GFCTNI-COD-SQL-ERRO
               MOVE  0926              TO GFCTNI-COD-MSG-ERRO
               MOVE '0926'             TO GFCTG2-COD-MSG
               MOVE 'S'                TO GFCTNI-FIM
               PERFORM 1400-OBTER-DESC-MSG
               MOVE  GFCTG3-DESC-MSG   TO GFCTNI-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           PERFORM 2561-OBTER-DESC-MUN.

      *----------------------------------------------------------------*
       2560-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ACESSA MODULO RURC9020                                          *
      ******************************************************************
      *----------------------------------------------------------------*
       2561-OBTER-DESC-MUN             SECTION.
      *----------------------------------------------------------------*

           INITIALIZE AREA-RURC88.

           MOVE CMUN-IBGE              OF GFCTB053
                                       TO COD-MUNIC-RURC88.
           MOVE 'RURC9020'             TO WRK-MODULO.

           CALL WRK-MODULO USING
                                             AREA-RURC88.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 1                  TO GFCTNI-ERRO
               MOVE ZEROS              TO GFCTNI-COD-SQL-ERRO
               MOVE  0732              TO GFCTNI-COD-MSG-ERRO
               MOVE '0732'             TO GFCTG2-COD-MSG
               PERFORM 1400-OBTER-DESC-MSG
               MOVE  GFCTG3-DESC-MSG   TO GFCTNI-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  COD-RETORNO-RURC88      EQUAL 10
               MOVE 1                  TO GFCTNI-ERRO
               MOVE ZEROS              TO GFCTNI-COD-SQL-ERRO
               MOVE  0675              TO GFCTNI-COD-MSG-ERRO
               MOVE '0675'             TO GFCTG2-COD-MSG
               PERFORM 1400-OBTER-DESC-MSG
               MOVE  GFCTG3-DESC-MSG   TO GFCTNI-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  COD-RETORNO-RURC88      EQUAL 20
               MOVE 1                  TO GFCTNI-ERRO
               MOVE ZEROS              TO GFCTNI-COD-SQL-ERRO
               MOVE  0733              TO GFCTNI-COD-MSG-ERRO
               MOVE '0733'             TO GFCTG2-COD-MSG
               PERFORM 1400-OBTER-DESC-MSG
               MOVE  GFCTG3-DESC-MSG   TO GFCTNI-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  COD-RETORNO-RURC88      EQUAL 99
               MOVE 1                  TO GFCTNI-ERRO
               MOVE ZEROS              TO GFCTNI-COD-SQL-ERRO
               MOVE  0734              TO GFCTNI-COD-MSG-ERRO
               MOVE '0734'             TO GFCTG2-COD-MSG
               PERFORM 1400-OBTER-DESC-MSG
               MOVE  GFCTG3-DESC-MSG   TO GFCTNI-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

0212       MOVE  CMUN-IBGE OF GFCTB053 TO WRK-CMUN-IBGE-S
0212
0212       IF  GFCTNH-FLT-CMUN-IBGE       EQUAL ZEROS      OR
0212           GFCTNH-FLT-CMUN-IBGE       EQUAL WRK-CMUN-IBGE
0212           PERFORM 2580-MONTAR-OCOR-SAIDA
0212           MOVE  WRK-CMUN-IBGE     TO GFCTNI-MUNICIPIO(IND-1)
0212           MOVE  NOME-MUNIC-RURC88(1:28) TO
                     GFCTNI-DESCR-AGPTO(IND-1)
BI1211     ELSE
BI1211         ADD 1                   TO WRK-QTDE-OCOR
BI1211         MOVE  WRK-QTDE-OCOR     TO GFCTNI-QTDE-OCOR
0212       END-IF.

      *----------------------------------------------------------------*
       2561-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ACESSA TABELA GFCTB057                                          *
      ******************************************************************
      *----------------------------------------------------------------*
       2570-SELECIONAR-UF              SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF           OF GFCTB049
                                       TO CSERVC-TARIF      OF GFCTB057.
           MOVE CAGPTO-CTA             OF GFCTB049
                                       TO CAGPTO-CTA        OF GFCTB057.
           MOVE HIDTFD-FLEXZ-AGPTO     OF GFCTB049
                                       TO HIDTFD-FLEXZ-AGPTO
                                                            OF GFCTB057.

           EXEC SQL
             SELECT
                   CSGL_UF
             INTO
                   :GFCTB057.CSGL-UF
             FROM   DB2PRD.FLEXZ_GRP_UF
             WHERE
                   CSERVC_TARIF       = :GFCTB057.CSERVC-TARIF       AND
                   CAGPTO_CTA         = :GFCTB057.CAGPTO-CTA         AND
                   HIDTFD_FLEXZ_AGPTO = :GFCTB057.HIDTFD-FLEXZ-AGPTO
           END-EXEC.

           IF (SQLCODE                     NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                    EQUAL 'W'      )
               MOVE SPACES                 TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'FLEXZ_GRP_UF'         TO GFCT0M-NOME-TAB
               MOVE 'SELECT'               TO GFCT0M-COMANDO-SQL
               MOVE '0120'                 TO GFCT0M-LOCAL
               MOVE  0010                  TO GFCTNI-COD-MSG-ERRO
               MOVE '0010'                 TO GFCTG2-COD-MSG
               MOVE 'GFCTB057'             TO WRK-NOM-TAB
               PERFORM 2110-FORMATAR-ERRO-DB2
           END-IF.

           IF  SQLCODE                     EQUAL +100
               MOVE  1                     TO GFCTNI-ERRO
               MOVE  ZEROS                 TO GFCTNI-COD-SQL-ERRO
               MOVE  0926                  TO GFCTNI-COD-MSG-ERRO
               MOVE '0926'                 TO GFCTG2-COD-MSG
               MOVE 'S'                    TO GFCTNI-FIM
               PERFORM 1400-OBTER-DESC-MSG
               MOVE  GFCTG3-DESC-MSG       TO GFCTNI-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

0212       IF  GFCTNH-FLT-CSGL-UF         EQUAL SPACES     OR
0212           GFCTNH-FLT-CSGL-UF         EQUAL CSGL-UF    OF GFCTB057
0212           PERFORM 2580-MONTAR-OCOR-SAIDA
0212           PERFORM 2571-OBTER-DESC-UF
0212           MOVE  WRK-AREA-DCLGEN-DCITV002
0212                                       TO DCITV002
0212           MOVE  IUF     OF DCITV002   TO GFCTNI-DESCR-AGPTO(IND-1)
0212           MOVE  CSGL-UF OF GFCTB057   TO GFCTNI-UF(IND-1)
BI1211     ELSE
BI1211         ADD 1                       TO WRK-QTDE-OCOR
BI1211         MOVE  WRK-QTDE-OCOR         TO GFCTNI-QTDE-OCOR
0212       END-IF.

      *----------------------------------------------------------------*
       2570-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ACESSA MODULO DCIT8000                                          *
      ******************************************************************
      *----------------------------------------------------------------*
       2571-OBTER-DESC-UF              SECTION.
      *----------------------------------------------------------------*

           MOVE CSGL-UF                OF GFCTB057
                                       TO CSGL-UF          OF DCITV002.
           MOVE DCITV002               TO WRK-AREA-DCLGEN-DCITV002.
           MOVE 'DCIT8000'             TO WRK-MODULO.

           CALL WRK-MODULO USING
                                             WRK-DCIT-UNIDADE-FEDERATIVA
                                             WRK-POOL7100
                                             WRK-SQLCA.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 1                  TO GFCTNI-ERRO
               MOVE ZEROS              TO GFCTNI-COD-SQL-ERRO
               MOVE  0735              TO GFCTNI-COD-MSG-ERRO
               MOVE '0735'             TO GFCTG2-COD-MSG
               PERFORM 1400-OBTER-DESC-MSG
               MOVE  GFCTG3-DESC-MSG   TO GFCTNI-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  WRK-DCIT-CODIGO-RETORNO EQUAL 1
               MOVE 1                  TO GFCTNI-ERRO
               MOVE ZEROS              TO GFCTNI-COD-SQL-ERRO
               MOVE  0736              TO GFCTNI-COD-MSG-ERRO
               MOVE '0736'             TO GFCTG2-COD-MSG
               PERFORM 1400-OBTER-DESC-MSG
               MOVE  GFCTG3-DESC-MSG   TO GFCTNI-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  WRK-DCIT-CODIGO-RETORNO EQUAL 2
               MOVE 1                  TO GFCTNI-ERRO
               MOVE ZEROS              TO GFCTNI-COD-SQL-ERRO
               MOVE  0737              TO GFCTNI-COD-MSG-ERRO
               MOVE '0737'             TO GFCTG2-COD-MSG
               PERFORM 1400-OBTER-DESC-MSG
               MOVE  GFCTG3-DESC-MSG   TO GFCTNI-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  WRK-DCIT-CODIGO-RETORNO EQUAL 3
               MOVE 1                  TO GFCTNI-ERRO
               MOVE ZEROS              TO GFCTNI-COD-SQL-ERRO
               MOVE  0738              TO GFCTNI-COD-MSG-ERRO
               MOVE '0738'             TO GFCTG2-COD-MSG
               PERFORM 1400-OBTER-DESC-MSG
               MOVE  GFCTG3-DESC-MSG   TO GFCTNI-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  WRK-DCIT-CODIGO-RETORNO EQUAL 4
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE 'V01_UF'           TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0130'             TO GFCT0M-LOCAL
               MOVE  0005              TO GFCTNI-COD-MSG-ERRO
               MOVE '0005'             TO GFCTG2-COD-MSG
               MOVE 9                  TO GFCTNI-ERRO
               MOVE 'GFCT0422'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE WRK-SQLCA          TO GFCT0M-SQLCA-AREA
                                          SQLCA
               MOVE SQLCODE            TO GFCT0M-SQLCODE
HEXA                                      WRK-COD-SQL-ERRO
HEXA           MOVE WRK-COD-SQL-ERRO-2 TO GFCTNI-COD-SQL-ERRO
               MOVE SPACES             TO GFCTNI-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2571-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

0212  ******************************************************************
0212  * MONTAR OCORRENCIAS DE SAIDA                                    *
0212  ******************************************************************
0212  *----------------------------------------------------------------*
0212   2580-MONTAR-OCOR-SAIDA          SECTION.
0212  *----------------------------------------------------------------*
0212
0212       ADD 1                       TO IND-1
0212
120510     COMPUTE GFCTNI-LL            = GFCTNI-LL + 178.
0212
0212       MOVE  GFCTNI-QTDE-TOT-REG   TO WRK-QTDE-TOT-REG
0212       MOVE  GFCTNI-QTDE-OCOR      TO WRK-QTDE-OCOR
0212
0212       ADD 1                       TO WRK-QTDE-OCOR
0212                                      WRK-QTDE-TOT-REG
0212
0212       MOVE  WRK-QTDE-TOT-REG      TO GFCTNI-QTDE-TOT-REG
0212       MOVE  WRK-QTDE-OCOR         TO GFCTNI-QTDE-OCOR
0212
0212       MOVE CSERVC-TARIF           OF GFCTB049
0212                                   TO WRK-CSERVC-TARIF-S
0212       MOVE WRK-CSERVC-TARIF       TO GFCTNI-COD-TARIFA   (IND-1)
0212       MOVE HIDTFD-FLEXZ-AGPTO     OF GFCTB049
0212                                   TO GFCTNI-TIMESTAMP    (IND-1)
0212       MOVE DINIC-FLEXZ-AGPTO      OF GFCTB049
0212                                   TO GFCTNI-DATA-INIC    (IND-1)
0212       MOVE DFIM-FLEXZ-AGPTO       OF GFCTB049
0212                                   TO GFCTNI-DATA-FIM     (IND-1)
0212
0212       PERFORM 2581-PESQUISAR-NOME-TARIFA
0212
0212       MOVE RSERVC-TARIF-REDZD     OF GFCTB0D8 (1:15)
0212                                   TO GFCTNI-DESCR-TARIFA (IND-1)
0212
0212       IF  GFCTNH-FLT-CAGPTO-CTA  NOT EQUAL  04 AND 09 AND 14 AND
0310                               15 AND 17 AND 18 AND 19 AND 20 AND 26
ST2506*        MOVE ZEROS              TO GFCTNI-CGC-CPF      (IND-1)
ST2506*                                TO GFCTNI-FILIAL       (IND-1)
0212           MOVE ZEROS              TO GFCTNI-SEGMENTO     (IND-1)
0212                                      GFCTNI-AGENCIA      (IND-1)
0212                                      GFCTNI-POSTO        (IND-1)
0212                                      GFCTNI-MUNICIPIO    (IND-1)
0212                                      GFCTNI-CONTROLE     (IND-1)
0310                                      GFCTNI-GRUPO-CTBIL  (IND-1)
0310                                      GFCTNI-SUB-GRUPO-CTBIL
0310                                                          (IND-1)
ST2506         MOVE SPACES             TO GFCTNI-CGC-CPF      (IND-1)
ST2506                                    GFCTNI-FILIAL       (IND-1)
0212                                      GFCTNI-UF           (IND-1)
0212                                      GFCTNI-DESCR-AGPTO  (IND-1)
120510                                    GFCTNI-DESC-CTBIL   (IND-1)
0212       END-IF.
0212
0212  *----------------------------------------------------------------*
0212   2580-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *EFETUA ACESSO NA TABELA GFCTB0D8                                *
      ******************************************************************
      *----------------------------------------------------------------*
       2581-PESQUISAR-NOME-TARIFA      SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF           OF GFCTB049
                                       TO CSERVC-TARIF
                                       OF GFCTB0D8.

           EXEC SQL
             SELECT
                   RSERVC_TARIF_REDZD
             INTO
                   :GFCTB0D8.RSERVC-TARIF-REDZD
             FROM   DB2PRD.SERVC_TARIF_PRINC
             WHERE
                   CSERVC_TARIF       = :GFCTB0D8.CSERVC-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'SERVC_TARIF_PRINC' TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0140'             TO GFCT0M-LOCAL
               MOVE  0010              TO GFCTNI-COD-MSG-ERRO
               MOVE '0010'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0D8'         TO WRK-NOM-TAB
               PERFORM 2110-FORMATAR-ERRO-DB2
           END-IF.

      *----------------------------------------------------------------*
       2581-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

0310  ******************************************************************
0310  *ACESSA TABELA PARA OBTER RAZAO                                  *
0310  ******************************************************************
0310  *----------------------------------------------------------------*
0710XS 2590-SELECIONAR-RAZAO           SECTION.
0310  *----------------------------------------------------------------*
0310
0310       MOVE CSERVC-TARIF           OF GFCTB049
0310                                   TO CSERVC-TARIF       OF GFCTB0M8
0310       MOVE CAGPTO-CTA             OF GFCTB049
0310                                   TO CAGPTO-CTA         OF GFCTB0M8
0310       MOVE HIDTFD-FLEXZ-AGPTO     OF GFCTB049
0310                                   TO HIDTFD-FLEXZ-AGPTO OF GFCTB0M8
0310
0310       EXEC SQL
0310         SELECT
0310               CGRP_CTBIL_ATUAL,
0310               CSGRP_CTBIL
0310         INTO
0310               :GFCTB0M8.CGRP-CTBIL-ATUAL,
0310               :GFCTB0M8.CSGRP-CTBIL
0310         FROM   DB2PRD.TFLEXZ_GRP_RZ
0310         WHERE
0310               CSERVC_TARIF       = :GFCTB0M8.CSERVC-TARIF       AND
0310               CAGPTO_CTA         = :GFCTB0M8.CAGPTO-CTA         AND
0310               HIDTFD_FLEXZ_AGPTO = :GFCTB0M8.HIDTFD-FLEXZ-AGPTO
0310       END-EXEC.
0310
0310       IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
0310          (SQLWARN0                EQUAL 'W'      )
0310           MOVE SPACES             TO GFCT0M-ERRO-SQL
0310           INITIALIZE GFCT0M-ERRO-SQL
0310           MOVE 'TFLEXZ_GRP_RZ  '  TO GFCT0M-NOME-TAB
0310           MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
0310           MOVE '0070'             TO GFCT0M-LOCAL
0310           MOVE  0010              TO GFCTNI-COD-MSG-ERRO
0310           MOVE '0010'             TO GFCTG2-COD-MSG
0310           MOVE 'GFCTB0M8'         TO WRK-NOM-TAB
0310           PERFORM 2110-FORMATAR-ERRO-DB2
0310       END-IF.
0310
0310       IF  SQLCODE                 EQUAL +100
0310           MOVE  1                 TO GFCTNI-ERRO
0310           MOVE  ZEROS             TO GFCTNI-COD-SQL-ERRO
0310           MOVE  0926              TO GFCTNI-COD-MSG-ERRO
0310           MOVE '0926'             TO GFCTG2-COD-MSG
0310           MOVE 'S'                TO GFCTNI-FIM
0310           PERFORM 1400-OBTER-DESC-MSG
0310           MOVE  GFCTG3-DESC-MSG   TO GFCTNI-DESC-MSG-ERRO
0310           PERFORM 3000-FINALIZAR
0310       END-IF.
0310
0310       MOVE  CGRP-CTBIL-ATUAL      OF GFCTB0M8
0310                                   TO WRK-CGRP-RZ-S.
0310       MOVE  CSGRP-CTBIL           OF GFCTB0M8
0310                                   TO WRK-CSGRP-RZ-S.
0310
0310       IF (GFCTNH-FLT-GRUPO-CTBIL      EQUAL ZEROS             AND
0310           GFCTNH-FLT-SUB-GRUPO-CTBIL  EQUAL ZEROS           )  OR
0310          (GFCTNH-FLT-GRUPO-CTBIL     NOT EQUAL ZEROS          AND
HEXA           GFCTNH-FLT-GRUPO-CTBIL     EQUAL WRK-CGRP-RZ        AND
0710XS         GFCTNH-FLT-SUB-GRUPO-CTBIL EQUAL WRK-CSGRP-RZ     )
0310           PERFORM 2580-MONTAR-OCOR-SAIDA
0310
0310           MOVE  WRK-CGRP-RZ           TO GFCTNI-GRUPO-CTBIL
0310                                                            (IND-1)
0310           MOVE  WRK-CSGRP-RZ          TO GFCTNI-SUB-GRUPO-CTBIL
0310                                                            (IND-1)
0310
110510         PERFORM 2591-BUSCAR-RAZAO-CONTABIL
0310
110510         MOVE  IRZ-CTBIL  OF PLCCB004 TO GFCTNI-DESC-CTBIL (IND-1)
BI1211     ELSE
BI1211         ADD 1                        TO WRK-QTDE-OCOR
BI1211         MOVE  WRK-QTDE-OCOR          TO GFCTNI-QTDE-OCOR
0310       END-IF.
0310
0310  *----------------------------------------------------------------*
0710XS 2590-99-FIM.                    EXIT.
0310  *----------------------------------------------------------------*

      ******************************************************************
      *ACESSAR PLCC PARA BUSCAR DESCRICAO RAZAO CONTABIL               *
      ******************************************************************
0510  *----------------------------------------------------------------*
0710XS 2591-BUSCAR-RAZAO-CONTABIL SECTION.
0510  *----------------------------------------------------------------*
0510
0510       EXEC SQL
0510           SELECT  IRZ_CTBIL
0510           INTO   :IRZ-CTBIL
0510           FROM    DB2PRD.RAZAO_CONTABIL
0510           WHERE   CEMPR = 237
0510           AND     CGRP_CTBIL_ATUAL =:GFCTB0M8.CGRP-CTBIL-ATUAL
0510           AND     CSGRP_CTBIL      =:GFCTB0M8.CSGRP-CTBIL
0510       END-EXEC.
0510
0510       IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
0510          (SQLWARN0                EQUAL 'W'      )
0510           INITIALIZE              GFCT0M-ERRO-SQL
0510           MOVE '0010'             TO GFCTG2-COD-MSG
0510           PERFORM 1400-OBTER-DESC-MSG
0510           MOVE 'PLCCB004'      TO WRK-NOM-TAB
0510           MOVE 'RAZAO_CONTABIL   '
0510                                   TO GFCT0M-NOME-TAB
0510           MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
0510           MOVE '0075'             TO GFCT0M-LOCAL
0510           MOVE  0010              TO GFCTNI-COD-MSG-ERRO
HEXA           MOVE SQLCODE            TO WRK-COD-SQL-ERRO
HEXA           MOVE WRK-COD-SQL-ERRO-2 TO GFCTNI-COD-SQL-ERRO
0510           MOVE  9                 TO GFCTNI-ERRO
               PERFORM 2110-FORMATAR-ERRO-DB2
0510           MOVE 'S'                TO GFCTNI-FIM
0510           MOVE GFCTG3-DESC-MSG    TO GFCTNI-DESC-MSG-ERRO
0510           PERFORM 3000-FINALIZAR
0510       END-IF.
0510
           IF  SQLCODE                 EQUAL +100
               MOVE 'NAO IDENTIFICADO' TO IRZ-CTBIL
           END-IF.

0510__*____IF__SQLCODE_________________EQUAL_+100
0510__*________MOVE__1_________________TO_GFCTNI-ERRO
0510__*________MOVE_ZEROS______________TO_GFCTNI-COD-SQL-ERRO
0510__*________MOVE__1690______________TO_GFCTNI-COD-MSG-ERRO
0510__*________MOVE_'1690'_____________TO_GFCTG2-COD-MSG
0310__*________PERFORM_1400-OBTER-DESC-MSG
0510__*________MOVE_'S'________________TO_GFCTNI-FIM
0510__*________MOVE_GFCTG3-DESC-MSG____TO_GFCTNI-DESC-MSG-ERRO
0510__*________PERFORM_3000-FINALIZAR
0510__*____END-IF.
0510
0510  *----------------------------------------------------------------*
0710XS 2591-99-FIM.                    EXIT.
0510  *----------------------------------------------------------------*

      ******************************************************************
      *FECHA O CURSOR DA TABELA GFCTB049                               *
      ******************************************************************
      *----------------------------------------------------------------*
       2600-FECHAR-CSR-GFCTB049        SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             CLOSE   CSR01-GFCTB049
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W'      )
               MOVE SPACES             TO GFCT0M-ERRO-SQL
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'FLEXZ_AGPTO_CTA'  TO GFCT0M-NOME-TAB
               MOVE 'CLOSE'            TO GFCT0M-COMANDO-SQL
               MOVE '0160'             TO GFCT0M-LOCAL
               MOVE  0011              TO GFCTNI-COD-MSG-ERRO
               MOVE '0011'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB049'         TO WRK-NOM-TAB
               PERFORM 2110-FORMATAR-ERRO-DB2
           END-IF.

      *----------------------------------------------------------------*
       2600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *FINALIZA O PROCESSAMENTO E RETORNA AO PROGRAMA CHAMADOR.        *
      ******************************************************************
      *----------------------------------------------------------------*
       3000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           GOBACK.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
