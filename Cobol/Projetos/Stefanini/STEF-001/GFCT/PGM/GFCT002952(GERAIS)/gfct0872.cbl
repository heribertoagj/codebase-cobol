      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT0872.
       AUTHOR.     WAGNER SILVA - PROCWORK.
      *================================================================*
      *                   C P M  -  S I S T E M A S                    *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT0872                                    *
      *    ANALISTA....:   WAGNER SILVA            - PROCWORK /  50    *
      *    DATA........:   03/11/2005                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   GERAR DADOS PARA DEMOSTRATIVO DIARIO DE     *
      *                    TARIFAS POR AGENCIA E CONTA.                *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *                DB2                                             *
      *                  TABLE                         INCLUDE/BOOK    *
      *                    DB2PRD.TMOVTO_TARIF_DIA       GFCTB0H6      *
      *                    DB2PRD.PARM_SERVC_TARIF       GFCTB0A2      *
      *                    DB2PRD.SERVC_TARIF_PRINC      GFCTB0D8      *
      *                    DB2PRD.TASSOC_CJTO_NATUZ      GFCTB0H2      *
      *                    DB2PRD.DEPDC_RLCDA            MESUB011      *
      *----------------------------------------------------------------*
      *    INC'S.......:                                               *
      *    I#GFCT9W - AREA DE COMUNICACAO - ENTRADA                    *
      *    I#GFCT9Y - AREA DE COMUNICACAO - SAIDA                      *
      *    I#GFCT0M - AREA DE COMUNICACAO - ERROS                      *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    GFCT5000 - OBTER DESCRICAO DE MENSAGENS                     *
      *    POOL0081 - ALOCAR DINAMICAMENTE UM MODULO                   *
      *    POOL1470 - CONSISTE DATA                                    *
      *================================================================*

      *================================================================*
HEXA  *
HEXA  *----------------------------------------------------------------*
HEXA  * ***  HEXASOLUTION - 03/2009 - CONVERSAO FAIXA DE AGENCIAS  *** *
HEXA  *----------------------------------------------------------------*
HEXA  *
           EJECT
      *================================================================*
       ENVIRONMENT                     DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *----------------------------------------------------------------*

       SPECIAL-NAMES.
           DECIMAL-POINT               IS COMMA.

           EJECT
      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** GFCT0872 - INICIO DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREAS PARA INDEXADORES ***'.
      *----------------------------------------------------------------*

       77  IND-1                       PIC  9(003)         VALUE ZEROS.
       77  WRK-PONT                    PIC  9(005)         VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREAS PARA VARIAVEIS AUXILIARES ***'.
      *----------------------------------------------------------------*

       77  WRK-FIM-CURSOR              PIC  X(001)         VALUE SPACES.
       77  WRK-MODULO                  PIC  X(008)         VALUE SPACES.
       77  WRK-TABELA                  PIC  X(008)         VALUE SPACES.

       01  WRK-CJTO-SERV-INI           PIC S9(003)V COMP-3 VALUE ZEROS.
       01  WRK-CJTO-SERV-FIM           PIC S9(003)V COMP-3 VALUE ZEROS.

       01  WRK-TARIFA-INI              PIC S9(005)V COMP-3 VALUE ZEROS.
       01  WRK-TARIFA-FIM              PIC S9(005)V COMP-3 VALUE ZEROS.

       01  WRK-TARIFA-ANT              PIC S9(005)V COMP-3 VALUE ZEROS.

       01  WRK-TIPO-RELAC              PIC 9(03)           VALUE ZEROS.
       01  WRK-SQLCODE                 PIC S9(05)          VALUE ZEROS.

       01  WRK-DATA-INI.
           03  WRK-ANO-INI             PIC  9(004)         VALUE ZEROS.
           03  WRK-MES-INI             PIC  9(002)         VALUE ZEROS.
           03  WRK-DIA-INI             PIC  9(002)         VALUE ZEROS.

       01  WRK-DATA-FIM.
           03  WRK-ANO-FIM             PIC  9(004)         VALUE ZEROS.
           03  WRK-MES-FIM             PIC  9(002)         VALUE ZEROS.
           03  WRK-DIA-FIM             PIC  9(002)         VALUE ZEROS.

       01  WRK-DTA-DB2-FIM.
           03  WRK-DIA-DB2-FIM         PIC  9(002)         VALUE ZEROS.
           03  FILLER                  PIC  X(001)         VALUE '.'.
           03  WRK-MES-DB2-FIM         PIC  9(002)         VALUE ZEROS.
           03  FILLER                  PIC  X(001)         VALUE '.'.
           03  WRK-ANO-DB2-FIM         PIC  9(004)         VALUE ZEROS.
       01  WRK-DATA-DB2-FIM            REDEFINES   WRK-DTA-DB2-FIM
                                       PIC  X(010).

       01  WRK-DTA-DB2-INI.
           03  WRK-DIA-DB2-INI         PIC  9(002)         VALUE ZEROS.
           03  FILLER                  PIC  X(001)         VALUE '.'.
           03  WRK-MES-DB2-INI         PIC  9(002)         VALUE ZEROS.
           03  FILLER                  PIC  X(001)         VALUE '.'.
           03  WRK-ANO-DB2-INI         PIC  9(004)         VALUE ZEROS.
       01  WRK-DATA-DB2-INI            REDEFINES   WRK-DTA-DB2-INI
                                       PIC  X(010).

           EJECT
      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA POOL1470 ***'.
      *---------------------------------------------------------------*

       01  WRK-DATA                    PIC  X(008)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA CAMPOS DO MODULO GFCT5000 ***'.
      *----------------------------------------------------------------*

       01  WRK-5000-AREA.
           03  WRK-5000-AREA-ENVIA.
               05  WRK-5000-AMBIENTE     PIC X(001)        VALUE SPACES.
               05  WRK-5000-COD-MSG      PIC 9(004)        VALUE ZEROS.
           03  WRK-5000-AREA-RECEBE.
               05  WRK-5000-COD-RETORNO  PIC 9(002)        VALUE ZEROS.
               05  WRK-5000-COD-SQL-ERRO PIC 9(003)        VALUE ZEROS.
               05  WRK-5000-DESC-MSG     PIC X(070)        VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*   AREA P/ MODULO GFCT5507    *'.
      *----------------------------------------------------------------*

       01  WRK-5507-AREA.
           03  WRK-5507-AREA-ENVIA.
               05  WRK-5507-AMBIENTE   PIC X(01)           VALUE SPACES.
               05  WRK-5507-CJUNC-DEPDC
                                       PIC 9(05)           VALUE ZEROS.
           03  WRK-5507-AREA-RECEBE.
               05  WRK-5507-COD-RETORNO
                                       PIC 9(02)           VALUE ZEROS.
               05  WRK-5507-COD-SQL-ERRO
                                       PIC 9(03)           VALUE ZEROS.
               05  WRK-5507-MSG-RETORNO
                                       PIC X(45)           VALUE SPACES.
               05  WRK-5507-IDTFD-MASTER
                                       PIC X(01)           VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*   AREA P/ MODULO SENH0315    *'.
      *----------------------------------------------------------------*

       01  WRK-VERSAO-0315             PIC  X(06)          VALUE
           'VRS001'.
       01  WRK-MENSAGEM-0315.
           03  WRK-COD-MENSAGEM-0315   PIC 9(03)           VALUE ZEROS.
           03  WRK-HIFEN-MENSAGEM-0315 PIC X(01)           VALUE SPACES.
           03  WRK-TEXTO-MENSAGEM-0315 PIC X(79)           VALUE SPACES.
       01  WRK-COD-FUNCIONAL-0315      PIC X(07)           VALUE SPACES.
       01  WRK-NOME-FUNC-0315          PIC X(40)           VALUE SPACES.
       01  WRK-COD-BANCO-0315          PIC 9(05)           VALUE ZEROS.
       01  WRK-NOME-BANCO-0315         PIC X(40)           VALUE SPACES.
       01  WRK-CNPJ-BANCO-0315.
           05  WRK-CNPJ-PRI-0315       PIC 9(09)           VALUE ZEROS.
           05  WRK-CNPJ-FIL-0315       PIC 9(05)           VALUE ZEROS.
           05  WRK-CNPJ-CON-0315       PIC 9(02)           VALUE ZEROS.
       01  WRK-COD-JUNCAO-0315         PIC 9(05)           VALUE ZEROS.
       01  WRK-NOME-DEPTO-0315         PIC X(40)           VALUE SPACES.
       01  WRK-SECAO-0315              PIC 9(03)           VALUE ZEROS.
       01  WRK-STATUS-0315             PIC X(01)           VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA MODULO MESU9025 ***'.
      *----------------------------------------------------------------*

       01  WRK-IO-PCB                  PIC X(32)           VALUE SPACES.
       01  WRK-ALT-PCB                 PIC X(32)           VALUE SPACES.
       01  WRK-MENSAGEM.
           03  WRK-MSG-COD             PIC 9(03)           VALUE ZEROS.
           03  FILLER                  PIC X(01)           VALUE SPACES.
           03  WRK-MSG-TXT             PIC X(79)           VALUE SPACES.
       01  WRK-RCB-TPO-RELA-X.
           03  WRK-RCB-TPO-RELA        PIC 9(03)           VALUE ZEROS.
       01  WRK-RCB-COD-EMPR-X.
           03  WRK-RCB-COD-EMPR        PIC 9(05)           VALUE ZEROS.
       01  WRK-RCB-COD-JUNC-X.
           03  WRK-RCB-COD-JUNC        PIC 9(05)           VALUE ZEROS.
       01  WRK-RCB-QTD-LIM-X.
           03  WRK-RCB-QTD-LIM         PIC 9(03)           VALUE ZEROS.
       01  WRK-RST-COD-EMPR-X.
           03  WRK-RST-COD-EMPR        PIC 9(05)           VALUE ZEROS.
       01  WRK-RST-COD-JUNC-X.
           03  WRK-RST-COD-JUNC        PIC 9(05)           VALUE ZEROS.
       01  WRK-DEV-QTD-RET             PIC 9(03)           VALUE ZEROS.
       01  WRK-DEV-TAB.
           03  WRK-DEV-TAB-OC          OCCURS 100 TIMES.
               05  WRK-DEV-OC-COD-EMPR PIC 9(05).
               05  WRK-DEV-OC-COD-JUNC PIC 9(05).

       01  WRK-MESUB010-NULL.
           03  WRK-CDIR-RGNAL-NULL     PIC S9(04)  COMP    VALUE +0.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE DB2 ***'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0H6
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0A2
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0D8
           END-EXEC.

           EXEC SQL
               INCLUDE GFCTB0H2
           END-EXEC.

           EXEC SQL
               INCLUDE MESUB011
           END-EXEC.

           EJECT
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** DECLARACAO DE CURSORES ***'.
      *----------------------------------------------------------------*

           EXEC SQL
             DECLARE   CRS-GFCTB0H6   CURSOR   FOR
               SELECT
                   T1.CAG_DSTNO_MOVTO,
                   T1.CCTA_DSTNO_MOVTO,
                   T1.CSEQ_MOVTO,
                   T1.DOCOR_EVNTO,
                   T1.CSERVC_TARIF,
                   T1.QEVNTO_AGRUP,
                   T1.VTARIF_CADTR_SIST,
                   T1.VTARIF_MOVTO_LIQ,
                   T1.VTARIF_MOVTO_DEB,
                   T1.VTARIF_RECBR_CLI,
                   T1.VTARIF_ESTRN_CLI,
                   T1.VTARIF_PERDA_CLI
             FROM  DB2PRD.TMOVTO_TARIF_DIA T1
             WHERE
                 ((T1.CAG_DSTNO_MOVTO    = :GFCTB0H6.CAG-DSTNO-MOVTO
             AND   T1.CCTA_DSTNO_MOVTO   = :GFCTB0H6.CCTA-DSTNO-MOVTO)
             AND  (T1.DOCOR_EVNTO       >= :WRK-DATA-DB2-INI
             AND   T1.DOCOR_EVNTO       <= :WRK-DATA-DB2-FIM)
             AND  (T1.CSERVC_TARIF      >= :WRK-TARIFA-INI
             AND   T1.CSERVC_TARIF      <= :WRK-TARIFA-FIM))
             AND   T1.CSERVC_TARIF      IN
                                          (SELECT DISTINCT
                                                  T2.CSERVC_TARIF
                                           FROM
                                           DB2PRD.PARM_SERVC_TARIF  T2,
                                           DB2PRD.TASSOC_CJTO_NATUZ T3
                                           WHERE  T2.CNATUZ_SERVC  =
                                                  T3.CNATUZ_SERVC
                                           AND    T3.CCJTO_SERVC  >=
                                                 :WRK-CJTO-SERV-INI
                                           AND    T3.CCJTO_SERVC  <=
                                                 :WRK-CJTO-SERV-FIM)
             ORDER BY
                   T1.DOCOR_EVNTO,
                   T1.CSERVC_TARIF
           END-EXEC.
HEXA  *
HEXA  *---------------------------------------------------------------*
HEXA  * AREA DE DECLARACAO PARA SUPORTE A MULTIPLAS FAIXAS DE AGENCIA *
HEXA  *---------------------------------------------------------------*
HEXA  *
HEXA       COPY I#HEXA01.
HEXA  *
HEXA       COPY I#BRAD7C.
HEXA  *

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** GFCT0872 - FIM DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       COPY I#GFCT9W.

           EJECT
       COPY I#GFCT9Y.

           EJECT
       COPY I#GFCT0M.

           EJECT
      *================================================================*
       PROCEDURE DIVISION              USING GFCT9W-ENTRADA
                                             GFCT9Y-SAIDA
                                             GFCT0M-AREA-ERROS.
      *================================================================*

      *----------------------------------------------------------------*
       000000-INICIAR                  SECTION.
      *----------------------------------------------------------------*
HEXA  *
HEXA  *---------------------------------------------------------------*
HEXA  * AREA DE MOVIMENTACAO DA ROTINA MESU E DE ABEND                *
HEXA  *---------------------------------------------------------------*
HEXA  *
HEXA       MOVE        'MESU9410'  TO          WRK-MODULO-MESUX0.
HEXA       MOVE        'BRAD7100'  TO          WRK-ABEND-MESUX0.
HEXA  *

           PERFORM 100000-INICIALIZAR.

           PERFORM 200000-PROCESSAR.

           PERFORM 300000-FINALIZAR.

      *----------------------------------------------------------------*
       000000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       100000-INICIALIZAR              SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCT0M-AREA-ERROS
                                          GFCT9Y-SAIDA
                                          WRK-FIM-CURSOR.

           INITIALIZE GFCT0M-AREA-ERROS
                      GFCT9Y-SAIDA.

           PERFORM 120000-INICIALIZA-AREA-SAIDA.

           PERFORM 110000-CONSISTIR-DADOS.

      *----------------------------------------------------------------*
       100000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       110000-CONSISTIR-DADOS          SECTION.
      *----------------------------------------------------------------*

           IF  GFCT9W-TRANSACAO            EQUAL SPACES     OR
               GFCT9W-TRANSACAO            EQUAL LOW-VALUES OR
               GFCT9W-FUNCAO               EQUAL SPACES     OR
               GFCT9W-FUNCAO               EQUAL LOW-VALUES OR
               GFCT9W-AGENCIA              NOT NUMERIC      OR
               GFCT9W-AGENCIA              EQUAL ZEROS      OR
               GFCT9W-CONTA                NOT NUMERIC      OR
               GFCT9W-CONTA                EQUAL ZEROS      OR
               GFCT9W-FUNC-BDSCO           EQUAL SPACES     OR
               GFCT9W-FUNC-BDSCO           EQUAL LOW-VALUES OR
               GFCT9W-PONTEIRO             EQUAL SPACES     OR
               GFCT9W-PONTEIRO             EQUAL LOW-VALUES OR
               GFCT9W-FIM                  NOT EQUAL 'S' AND 'N'
               MOVE 1                      TO GFCT9Y-ERRO
                                              GFCT9Y-COD-MSG-ERRO
                                              WRK-5000-COD-MSG
               MOVE 'S'                    TO GFCT9Y-FIM
               MOVE ZEROS                  TO GFCT9Y-COD-SQL-ERRO
               PERFORM 111000-OBTER-DESCRICAO-MSG
               PERFORM 300000-FINALIZAR
           END-IF.

           IF  GFCT9W-DATA-INI(07:04)      LESS    1900    OR
               GFCT9W-DATA-INI(07:04)      GREATER 2100    OR
               GFCT9W-DATA-FIM(07:04)      LESS    1900    OR
               GFCT9W-DATA-FIM(07:04)      GREATER 2100
               MOVE 1                      TO GFCT9Y-ERRO
               MOVE 0200                   TO GFCT9Y-COD-MSG-ERRO
                                              WRK-5000-COD-MSG
               MOVE 'S'                    TO GFCT9Y-FIM
               MOVE ZEROS                  TO GFCT9Y-COD-SQL-ERRO
               PERFORM 111000-OBTER-DESCRICAO-MSG
               PERFORM 300000-FINALIZAR
           END-IF.

           MOVE GFCT9W-DATA-FIM(01:02)     TO WRK-DIA-FIM.
           MOVE GFCT9W-DATA-FIM(04:02)     TO WRK-MES-FIM.
           MOVE GFCT9W-DATA-FIM(07:04)     TO WRK-ANO-FIM.
           MOVE GFCT9W-DATA-INI(01:02)     TO WRK-DIA-INI.
           MOVE GFCT9W-DATA-INI(04:02)     TO WRK-MES-INI.
           MOVE GFCT9W-DATA-INI(07:04)     TO WRK-ANO-INI.

           MOVE GFCT9W-DATA-INI(01:02)     TO WRK-DATA(1:2).
           MOVE GFCT9W-DATA-INI(04:02)     TO WRK-DATA(3:2).
           MOVE GFCT9W-DATA-INI(07:04)     TO WRK-DATA(5:4).

           PERFORM 112000-CONSISTIR-DATA.

           MOVE GFCT9W-DATA-FIM(01:02)     TO WRK-DATA(1:2).
           MOVE GFCT9W-DATA-FIM(04:02)     TO WRK-DATA(3:2).
           MOVE GFCT9W-DATA-FIM(07:04)     TO WRK-DATA(5:4).

           PERFORM 112000-CONSISTIR-DATA.

           IF  WRK-DATA-FIM                LESS WRK-DATA-INI
               MOVE 1                      TO GFCT9Y-ERRO
               MOVE 0201                   TO GFCT9Y-COD-MSG-ERRO
                                              WRK-5000-COD-MSG
               MOVE 'S'                    TO GFCT9Y-FIM
               MOVE ZEROS                  TO GFCT9Y-COD-SQL-ERRO
               PERFORM 111000-OBTER-DESCRICAO-MSG
               PERFORM 300000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       110000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       112000-CONSISTIR-DATA           SECTION.
      *----------------------------------------------------------------*

           CALL  'POOL1470'            USING WRK-DATA.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 1                      TO GFCT9Y-ERRO
               MOVE 0200                   TO GFCT9Y-COD-MSG-ERRO
                                              WRK-5000-COD-MSG
               MOVE 'S'                    TO GFCT9Y-FIM
               MOVE ZEROS                  TO GFCT9Y-COD-SQL-ERRO
               PERFORM 111000-OBTER-DESCRICAO-MSG
               PERFORM 300000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       112000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       111000-OBTER-DESCRICAO-MSG      SECTION.
      *----------------------------------------------------------------*

           MOVE 'O'                         TO WRK-5000-AMBIENTE.
           MOVE 'GFCT5000'                  TO WRK-MODULO.

           CALL 'POOL0081'                  USING WRK-MODULO
                                                  WRK-5000-AREA-ENVIA
                                                  WRK-5000-AREA-RECEBE.

           IF  WRK-5000-COD-RETORNO         NOT EQUAL ZEROS
               IF  WRK-5000-COD-RETORNO     EQUAL 01
                   MOVE 9                   TO GFCT9Y-ERRO
                   MOVE ZEROS               TO GFCT9Y-COD-SQL-ERRO
                                               GFCT9Y-COD-MSG-ERRO
                   MOVE WRK-5000-DESC-MSG   TO GFCT9Y-DESC-MSG-ERRO
               ELSE
                   IF  WRK-5000-COD-RETORNO EQUAL 99
                       MOVE 9               TO GFCT9Y-ERRO
                       MOVE WRK-5000-COD-SQL-ERRO
                                            TO GFCT9Y-COD-SQL-ERRO
                       MOVE ZEROS           TO GFCT9Y-COD-MSG-ERRO
                       MOVE WRK-5000-DESC-MSG  TO GFCT9Y-DESC-MSG-ERRO
                   END-IF
               END-IF
               INITIALIZE GFCT9Y-DADOS
               PERFORM 300000-FINALIZAR
           END-IF.

           MOVE WRK-5000-DESC-MSG           TO GFCT9Y-DESC-MSG-ERRO.

      *----------------------------------------------------------------*
       111000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       120000-INICIALIZA-AREA-SAIDA    SECTION.
      *----------------------------------------------------------------*

           IF  GFCT9W-QTDE-OCOR        NOT NUMERIC
               MOVE ZEROS              TO GFCT9Y-QTDE-OCOR
           ELSE
               MOVE GFCT9W-QTDE-OCOR   TO GFCT9Y-QTDE-OCOR
           END-IF.

           IF  GFCT9W-QTDE-TOT-REG     NOT NUMERIC
               MOVE ZEROS              TO GFCT9Y-QTDE-TOT-REG
           ELSE
               MOVE GFCT9W-QTDE-TOT-REG
                                       TO GFCT9Y-QTDE-TOT-REG
           END-IF.

           IF  GFCT9W-COD-TARIFA       NOT NUMERIC OR
               GFCT9W-COD-TARIFA       EQUAL ZEROS
               MOVE ZEROS              TO GFCT9Y-COD-TARIFA
                                          WRK-TARIFA-INI
               MOVE 999999             TO WRK-TARIFA-FIM
           ELSE
               MOVE GFCT9W-COD-TARIFA  TO GFCT9Y-COD-TARIFA
                                          WRK-TARIFA-INI
                                          WRK-TARIFA-FIM
           END-IF.

           MOVE GFCT9W-LL              TO GFCT9Y-LL.
           MOVE GFCT9W-ZZ              TO GFCT9Y-ZZ.
           MOVE GFCT9W-TRANSACAO       TO GFCT9Y-TRANSACAO.
           MOVE GFCT9W-FUNCAO          TO GFCT9Y-FUNCAO.
           MOVE GFCT9W-AGENCIA         TO GFCT9Y-AGENCIA.
           MOVE GFCT9W-CONTA           TO GFCT9Y-CONTA.
           MOVE GFCT9W-DATA-INI        TO GFCT9Y-DATA-INI.
           MOVE GFCT9W-DATA-FIM        TO GFCT9Y-DATA-FIM.
           MOVE GFCT9W-NOME-CLIENTE    TO GFCT9Y-NOME-CLIENTE.
           MOVE GFCT9W-NOME-TARIFA     TO GFCT9Y-NOME-TARIFA.
           MOVE GFCT9W-DEPTO-BDSCO     TO GFCT9Y-DEPTO-BDSCO.
           MOVE GFCT9W-FUNC-BDSCO      TO GFCT9Y-FUNC-BDSCO.
           MOVE GFCT9W-TERMINAL        TO GFCT9Y-TERMINAL.
           MOVE GFCT9W-PONTEIRO        TO GFCT9Y-PONTEIRO.
           MOVE GFCT9W-FIM             TO GFCT9Y-FIM.

           IF  GFCT9W-COD-CJTO-SERVICO NOT NUMERIC OR
               GFCT9W-COD-CJTO-SERVICO EQUAL ZEROS
               MOVE ZEROS              TO GFCT9Y-COD-CJTO-SERVICO
                                          WRK-CJTO-SERV-INI
               MOVE 999                TO WRK-CJTO-SERV-FIM
           ELSE
               MOVE GFCT9W-COD-CJTO-SERVICO
                                       TO GFCT9Y-COD-CJTO-SERVICO
                                          WRK-CJTO-SERV-INI
                                          WRK-CJTO-SERV-FIM
           END-IF.

           MOVE 290                    TO GFCT9Y-LL.
           MOVE ZEROS                  TO GFCT9Y-ERRO
                                          GFCT9Y-COD-SQL-ERRO
                                          GFCT9Y-COD-MSG-ERRO
                                          WRK-5000-COD-MSG.

           PERFORM 111000-OBTER-DESCRICAO-MSG.

      *----------------------------------------------------------------*
       120000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       200000-PROCESSAR                SECTION.
      *----------------------------------------------------------------*

           PERFORM 201000-CONSISTENCIA.

           PERFORM 210000-ABRIR-CRS-GFCTB0H6.

           PERFORM 220000-LER-CRS-GFCTB0H6
               UNTIL WRK-FIM-CURSOR    EQUAL 'S' OR
               WRK-PONT                GREATER GFCT9Y-PONTEIRO.

           PERFORM 230000-PROCESSAR-CRS-CFCTB0C3
               VARYING IND-1           FROM 1 BY 1
               UNTIL WRK-FIM-CURSOR    EQUAL 'S' OR
               IND-1                   GREATER 11.

           IF  IND-1                   GREATER 11
               COMPUTE WRK-PONT = (WRK-PONT - 1)
               MOVE WRK-PONT           TO GFCT9Y-PONTEIRO
           END-IF.

           PERFORM 240000-FECHAR-CRS-GFCTB0H6.

      *----------------------------------------------------------------*
       200000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       201000-CONSISTENCIA              SECTION.
      *----------------------------------------------------------------*

           PERFORM 201100-OBTER-COD-JUNCAO.

           PERFORM 201200-VERIFICAR-MASTER.

      *----------------->>> MASTER
           IF  WRK-5507-IDTFD-MASTER   EQUAL 'S'
               GO                      TO  201000-99-FIM
           END-IF.

      *----------------->>> DEPTO DO USUARIO = DEPTO A ACESSAR
           IF  GFCT9W-DEPTO-BDSCO       = GFCT9W-AGENCIA
               GO                      TO 201000-99-FIM
           END-IF.

      *----------------->>> GERENCIA REGIONAL
           MOVE 17            TO WRK-TIPO-RELAC
           PERFORM 201300-VERIFICAR-DIRET-GER
           IF  SQLCODE     EQUAL ZERO   OR -811
               GO                  TO   201000-99-FIM
           END-IF.

      *----------------->>> DIRETORIA REGIONAL
           MOVE 11            TO WRK-TIPO-RELAC
           PERFORM 201300-VERIFICAR-DIRET-GER
           IF  SQLCODE     EQUAL ZERO   OR -811
               GO                  TO   201000-99-FIM
           END-IF.

      *----------------->>>CONDICAO DE ERRO
           MOVE 1                 TO GFCT9Y-ERRO
           MOVE ZEROS             TO GFCT9Y-COD-SQL-ERRO
           MOVE 0213              TO GFCT9Y-COD-MSG-ERRO
                                     WRK-5000-COD-MSG
           MOVE 'S'               TO GFCT9Y-FIM
           PERFORM 111000-OBTER-DESCRICAO-MSG
           MOVE WRK-5000-DESC-MSG TO GFCT9Y-DESC-MSG-ERRO
           PERFORM 300000-FINALIZAR.

      *----------------------------------------------------------------*
       201000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       201300-VERIFICAR-DIRET-GER       SECTION.
      *----------------------------------------------------------------*

      *--------------->> SO PARA DEPARTAMENTOS
HEXA  *----------------------------------------------------------------*
HEXA  *            INICIO     DO     TEMPLATE_IF
HEXA  *----------------------------------------------------------------*
HEXA  ***  ALTERADO PARA SUPORTE A MULTIPLAS FAIXAS DE AGENCIA
HEXA  *
HEXA  *    HX_IF   GFCT9W-DEPTO-BDSCO   <  4000
HEXA  *
HEXA          MOVE     GFCT9W-DEPTO-BDSCO TO   WRK-MESUX0-AGENCIA
HEXA          PERFORM  9990-10-CHAMA-ROTINA-FX-AG
HEXA  *
HEXA          IF   WRK-EH-FX-AG
HEXA  *
HEXA  *----------------------------------------------------------------*
HEXA  *            TERMINO    DO     TEMPLATE_IF
HEXA  *----------------------------------------------------------------*
HEXA  *
                   MOVE +100           TO  SQLCODE
                   GO                  TO  201300-99-FIM.

      *--------------->> VERIFICA SE USUARIO E GER OU DIRET REGIONAL
              MOVE WRK-TIPO-RELAC      TO  CTPO-DEPDC-RLCDA OF MESUB011.
              MOVE GFCT9W-DEPTO-BDSCO  TO  CDEPDC-RLCDA     OF MESUB011.
              MOVE GFCT9W-AGENCIA      TO  CDEPDC           OF MESUB011.

              EXEC SQL
                SELECT  CEMPR_INC,
                        CDEPDC
                INTO    :MESUB011.CEMPR-INC,
                        :MESUB011.CDEPDC
                FROM  DB2PRD.DEPDC_RLCDA
                  WHERE  CTPO_DEPDC_RLCDA = :MESUB011.CTPO-DEPDC-RLCDA
                    AND  CDEPDC_RLCDA     = :MESUB011.CDEPDC-RLCDA
                    AND  CDEPDC           = :MESUB011.CDEPDC
                    AND  CEMPR_INC_RLCDA  = 237
                    AND  CEMPR_INC        = 237
              END-EXEC.

           MOVE     SQLCODE          TO   WRK-SQLCODE.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100
                                                       AND -811) OR
               SQLWARN0                EQUAL 'W'
               MOVE 'GFCT0872'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'DEPDC_DEPDC_RLCDA '
                                       TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-STACODE
                                          GFCT9Y-COD-SQL-ERRO
               MOVE '0001'             TO GFCT0M-LOCAL
               MOVE SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE 9                  TO GFCT9Y-ERRO
               MOVE 0012               TO GFCT9Y-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               PERFORM 111000-OBTER-DESCRICAO-MSG
               MOVE ' - MESUB011'      TO WRK-TABELA
               PERFORM 211000-CONCATENAR-MENSAGEM
               PERFORM 300000-FINALIZAR.

      *----------------------------------------------------------------*
       201300-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       201100-OBTER-COD-JUNCAO          SECTION.
      *----------------------------------------------------------------*

           MOVE 'VRS001'               TO WRK-VERSAO-0315.
           MOVE GFCT9W-FUNC-BDSCO      TO WRK-COD-FUNCIONAL-0315.

           CALL 'SENH0315'             USING WRK-VERSAO-0315
                                             WRK-MENSAGEM-0315
                                             WRK-COD-FUNCIONAL-0315
                                             WRK-NOME-FUNC-0315
                                             WRK-COD-BANCO-0315
                                             WRK-NOME-BANCO-0315
                                             WRK-CNPJ-BANCO-0315
                                             WRK-COD-JUNCAO-0315
                                             WRK-NOME-DEPTO-0315
                                             WRK-SECAO-0315
                                             WRK-STATUS-0315.

           IF  RETURN-CODE             NOT EQUAL ZEROS AND 04
               MOVE 1                  TO GFCT9Y-ERRO
               MOVE RETURN-CODE        TO GFCT9Y-COD-SQL-ERRO
               MOVE 0056               TO GFCT9Y-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               PERFORM 111000-OBTER-DESCRICAO-MSG
               MOVE WRK-5000-DESC-MSG  TO GFCT9Y-DESC-MSG-ERRO
               PERFORM 300000-FINALIZAR
           END-IF.

           IF  RETURN-CODE             EQUAL 04
               IF  WRK-COD-MENSAGEM-0315
                                       EQUAL 001
                   MOVE 1              TO GFCT9Y-ERRO
                   MOVE ZEROS          TO GFCT9Y-COD-SQL-ERRO
                   MOVE 0057           TO GFCT9Y-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
                   PERFORM 111000-OBTER-DESCRICAO-MSG
                   MOVE WRK-5000-DESC-MSG
                                       TO GFCT9Y-DESC-MSG-ERRO
                   PERFORM 300000-FINALIZAR
               END-IF
               IF  WRK-COD-MENSAGEM-0315
                                       EQUAL 154
                   MOVE 1              TO GFCT9Y-ERRO
                   MOVE ZEROS          TO GFCT9Y-COD-SQL-ERRO
                   MOVE 0058           TO GFCT9Y-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
                   PERFORM 111000-OBTER-DESCRICAO-MSG
                   MOVE WRK-5000-DESC-MSG
                                       TO GFCT9Y-DESC-MSG-ERRO
                   PERFORM 300000-FINALIZAR
               END-IF
               IF  WRK-COD-MENSAGEM-0315
                                       EQUAL 242
                   MOVE 1              TO GFCT9Y-ERRO
                   MOVE ZEROS          TO GFCT9Y-COD-SQL-ERRO
                   MOVE 0059           TO GFCT9Y-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
                   PERFORM 111000-OBTER-DESCRICAO-MSG
                   MOVE WRK-5000-DESC-MSG
                                       TO GFCT9Y-DESC-MSG-ERRO
                   PERFORM 300000-FINALIZAR
               END-IF
               IF  WRK-COD-MENSAGEM-0315
                                       EQUAL 009
                   MOVE ZEROS          TO GFCT9Y-ERRO
                                          GFCT9Y-COD-SQL-ERRO
                                          GFCT9Y-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
                   PERFORM 111000-OBTER-DESCRICAO-MSG
                   MOVE WRK-5000-DESC-MSG
                                       TO GFCT9Y-DESC-MSG-ERRO
                   PERFORM 300000-FINALIZAR
               END-IF
               IF  WRK-COD-MENSAGEM-0315
                                       EQUAL 138
                   MOVE 1              TO GFCT9Y-ERRO
                   MOVE ZEROS          TO GFCT9Y-COD-SQL-ERRO
                   MOVE 0060           TO GFCT9Y-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
                   PERFORM 111000-OBTER-DESCRICAO-MSG
                   MOVE WRK-5000-DESC-MSG
                                       TO GFCT9Y-DESC-MSG-ERRO
                   PERFORM 300000-FINALIZAR
               END-IF
           END-IF.

               MOVE WRK-COD-JUNCAO-0315   TO GFCT9Y-DEPTO-BDSCO
                                             GFCT9W-DEPTO-BDSCO.

      *----------------------------------------------------------------*
       201100-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       201200-VERIFICAR-MASTER          SECTION.
      *----------------------------------------------------------------*

           MOVE 'O'                    TO WRK-5507-AMBIENTE.
           MOVE WRK-COD-JUNCAO-0315    TO WRK-5507-CJUNC-DEPDC.
           MOVE 'GFCT5507'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             WRK-5507-AREA.

           IF  WRK-5507-COD-RETORNO    NOT EQUAL ZEROS
               IF  WRK-5507-COD-RETORNO
                                       EQUAL 01
                   MOVE 1              TO GFCT9Y-ERRO
                   MOVE ZEROS          TO GFCT9Y-COD-SQL-ERRO
                                          GFCT9Y-COD-MSG-ERRO
                   MOVE WRK-5507-MSG-RETORNO
                                       TO GFCT9Y-DESC-MSG-ERRO
               ELSE
                   IF  WRK-5507-COD-RETORNO
                                       EQUAL 99
                       MOVE 1          TO GFCT9Y-ERRO
                       MOVE WRK-5507-COD-SQL-ERRO
                                       TO GFCT9Y-COD-SQL-ERRO
                       MOVE ZEROS      TO GFCT9Y-COD-MSG-ERRO
                       MOVE WRK-5507-MSG-RETORNO
                                       TO GFCT9Y-DESC-MSG-ERRO
                   END-IF
               END-IF
               PERFORM 300000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       201200-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       210000-ABRIR-CRS-GFCTB0H6       SECTION.
      *----------------------------------------------------------------*

           MOVE GFCT9W-AGENCIA         TO CAG-DSTNO-MOVTO  OF GFCTB0H6.
           MOVE GFCT9W-CONTA           TO CCTA-DSTNO-MOVTO OF GFCTB0H6.
           MOVE GFCT9Y-DATA-INI(1:2)   TO WRK-DIA-DB2-INI.
           MOVE GFCT9Y-DATA-INI(4:2)   TO WRK-MES-DB2-INI.
           MOVE GFCT9Y-DATA-INI(7:4)   TO WRK-ANO-DB2-INI.
           MOVE GFCT9Y-DATA-FIM(1:2)   TO WRK-DIA-DB2-FIM.
           MOVE GFCT9Y-DATA-FIM(4:2)   TO WRK-MES-DB2-FIM.
           MOVE GFCT9Y-DATA-FIM(7:4)   TO WRK-ANO-DB2-FIM.

           EXEC SQL
                   OPEN    CRS-GFCTB0H6
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'GFCT0872'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'TMOVTO_TARIFA_CTA' TO GFCT0M-NOME-TAB
               MOVE 'OPEN'             TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE           TO GFCT0M-STACODE
                                          GFCT9Y-COD-SQL-ERRO
               MOVE '0002'             TO GFCT0M-LOCAL
               MOVE  SQLCA             TO GFCT0M-SQLCA-AREA
               MOVE 9                  TO GFCT9Y-ERRO
               MOVE 0005               TO GFCT9Y-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               PERFORM 111000-OBTER-DESCRICAO-MSG
               MOVE 'GFCTB0H6'         TO WRK-TABELA
               PERFORM 211000-CONCATENAR-MENSAGEM
               PERFORM 300000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       210000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       211000-CONCATENAR-MENSAGEM      SECTION.
      *----------------------------------------------------------------*

           STRING  WRK-5000-DESC-MSG
                  ' - '
                   WRK-TABELA
           DELIMITED BY '  '           INTO GFCT9Y-DESC-MSG-ERRO.

      *----------------------------------------------------------------*
       211000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       220000-LER-CRS-GFCTB0H6         SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             FETCH   CRS-GFCTB0H6   INTO
                   :GFCTB0H6.CAG-DSTNO-MOVTO,
                   :GFCTB0H6.CCTA-DSTNO-MOVTO,
                   :GFCTB0H6.CSEQ-MOVTO,
                   :GFCTB0H6.DOCOR-EVNTO,
                   :GFCTB0H6.CSERVC-TARIF,
                   :GFCTB0H6.QEVNTO-AGRUP,
                   :GFCTB0H6.VTARIF-CADTR-SIST,
                   :GFCTB0H6.VTARIF-MOVTO-LIQ,
                   :GFCTB0H6.VTARIF-MOVTO-DEB,
                   :GFCTB0H6.VTARIF-RECBR-CLI,
                   :GFCTB0H6.VTARIF-ESTRN-CLI,
                   :GFCTB0H6.VTARIF-PERDA-CLI
           END-EXEC.


           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'GFCT0872'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'TMOVTO_TARIFA_CTA' TO GFCT0M-NOME-TAB
               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE           TO GFCT0M-STACODE
                                          GFCT9Y-COD-SQL-ERRO
               MOVE '0003'             TO GFCT0M-LOCAL
               MOVE  SQLCA             TO GFCT0M-SQLCA-AREA
               MOVE 9                  TO GFCT9Y-ERRO
               MOVE 0006               TO GFCT9Y-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               PERFORM 111000-OBTER-DESCRICAO-MSG
               MOVE 'GFCTB0H6'         TO WRK-TABELA
               PERFORM 211000-CONCATENAR-MENSAGEM
               PERFORM 300000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FIM-CURSOR
                                          GFCT9Y-FIM
               IF  IND-1               EQUAL ZEROS
                   MOVE 1              TO GFCT9Y-ERRO
                                          GFCT9Y-COD-SQL-ERRO
                   MOVE 0202           TO GFCT9Y-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
                   PERFORM 111000-OBTER-DESCRICAO-MSG
                   MOVE 'GFCTB0H6'     TO WRK-TABELA
                   PERFORM 211000-CONCATENAR-MENSAGEM
               ELSE
                   MOVE ZEROS          TO GFCT9Y-ERRO
                                          GFCT9Y-COD-SQL-ERRO
                   MOVE 0007           TO GFCT9Y-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
                   PERFORM 111000-OBTER-DESCRICAO-MSG
               END-IF
           END-IF.

           ADD 1                   TO    WRK-PONT.

      *----------------------------------------------------------------*
       220000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       230000-PROCESSAR-CRS-CFCTB0C3   SECTION.
      *----------------------------------------------------------------*

           COMPUTE GFCT9Y-LL           = GFCT9Y-LL + 90.
           ADD  1                      TO GFCT9Y-QTDE-TOT-REG.

           MOVE DOCOR-EVNTO            OF GFCTB0H6
                                       TO GFCT9Y-DATA-OCOR(IND-1).
           MOVE CSERVC-TARIF           OF GFCTB0H6
                                       TO GFCT9Y-TARIFA-OCOR(IND-1).

           IF CSERVC-TARIF OF GFCTB0H6
                                   NOT EQUAL WRK-TARIFA-ANT
              MOVE CSERVC-TARIF OF GFCTB0H6
                                       TO WRK-TARIFA-ANT

              PERFORM 231000-OBTER-DESC-TARIFA
           END-IF.

           MOVE RSERVC-TARIF-REDZD     OF GFCTB0D8
                                      TO GFCT9Y-NOME-TARIFA-OCOR(IND-1).

           MOVE QEVNTO-AGRUP           OF GFCTB0H6
                                      TO GFCT9Y-QUANTIDDADE-OCOR(IND-1).
           MOVE VTARIF-CADTR-SIST      OF GFCTB0H6
                                       TO GFCT9Y-POTENCIAL-OCOR(IND-1).
           MOVE VTARIF-MOVTO-LIQ       OF GFCTB0H6
                                       TO GFCT9Y-CALCULADO-OCOR(IND-1).
           MOVE VTARIF-MOVTO-DEB       OF GFCTB0H6
                                       TO GFCT9Y-RECEBIDO-OCOR(IND-1).
           MOVE VTARIF-RECBR-CLI       OF GFCTB0H6
                                       TO GFCT9Y-RECEBER-OCOR(IND-1).
           MOVE VTARIF-ESTRN-CLI       OF GFCTB0H6
                                       TO GFCT9Y-ESTORNADO-OCOR(IND-1).
           MOVE VTARIF-PERDA-CLI       OF GFCTB0H6
                                       TO GFCT9Y-PERDA-OCOR(IND-1).

           PERFORM 220000-LER-CRS-GFCTB0H6.

      *----------------------------------------------------------------*
       230000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       231000-OBTER-DESC-TARIFA        SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF OF GFCTB0H6
                                       TO CSERVC-TARIF OF GFCTB0D8.

           EXEC SQL
               SELECT
                       RSERVC_TARIF_REDZD
               INTO
                       :GFCTB0D8.RSERVC-TARIF-REDZD
               FROM    DB2PRD.SERVC_TARIF_PRINC
               WHERE   CSERVC_TARIF     = :GFCTB0D8.CSERVC-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100 AND
                                                 -811) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'GFCT0872'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'SERVC_TARIF_PRINC' TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE           TO GFCT0M-STACODE
                                          GFCT9Y-COD-SQL-ERRO
               MOVE '0004'             TO GFCT0M-LOCAL
               MOVE  SQLCA             TO GFCT0M-SQLCA-AREA
               MOVE 9                  TO GFCT9Y-ERRO
               MOVE 0009               TO GFCT9Y-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               PERFORM 111000-OBTER-DESCRICAO-MSG
               MOVE 'GFCTB0D8'         TO WRK-TABELA
               PERFORM 211000-CONCATENAR-MENSAGEM
               PERFORM 300000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       231000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       240000-FECHAR-CRS-GFCTB0H6      SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             CLOSE   CRS-GFCTB0H6
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'GFCT0872'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'TMOVTO_TARIFA_CTA' TO GFCT0M-NOME-TAB
               MOVE 'CLOSE'            TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE           TO GFCT0M-STACODE
                                          GFCT9Y-COD-SQL-ERRO
               MOVE '0005'             TO GFCT0M-LOCAL
               MOVE  SQLCA             TO GFCT0M-SQLCA-AREA
               MOVE 9                  TO GFCT9Y-ERRO
               MOVE 0011               TO GFCT9Y-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               PERFORM 111000-OBTER-DESCRICAO-MSG
               MOVE 'GFCTB0H6'         TO WRK-TABELA
               PERFORM 211000-CONCATENAR-MENSAGEM
               PERFORM 300000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       240000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       300000-FINALIZAR                SECTION.
      *----------------------------------------------------------------*

           GOBACK.

      *----------------------------------------------------------------*
       300000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
HEXA  *----------------------------------------------------------------*
HEXA  * SECTION PARA CHAMADA DA ROTINA DE CONVERSAO DE AGENCIA
HEXA  *----------------------------------------------------------------*
HEXA  *
HEXA  *----------------------------------------------------------------*
HEXA   9990-10-CHAMA-ROTINA-FX-AG SECTION.
HEXA  *----------------------------------------------------------------*
HEXA  *
HEXA       CALL        WRK-MODULO-MESUX0 USING WRK-AREA-MESUX0.
HEXA  *
HEXA       IF          WRK-EH-ERRO-MESU-ABEND
HEXA           MOVE    'APL'       TO          ERR-TIPO-ACESSO
HEXA           MOVE    'ERRO NA CHAMADA DA ROTINA DE FAIXAS DE AGENCIA'
HEXA             TO    ERR-TEXTO
HEXA           PERFORM 9991-10-CHAMA-ROTINA-ABEND
HEXA       ELSE
HEXA           MOVE    ZEROS       TO          WRK-MESUX0-AGENCIA
HEXA       END-IF.
HEXA  *
HEXA  *----------------------------------------------------------------*
HEXA   9990-90-EXIT. EXIT.
HEXA  *----------------------------------------------------------------*
HEXA  *
HEXA  *----------------------------------------------------------------*
HEXA  * SECTION PARA CHAMADA DA ROTINA DE ABEND
HEXA  *----------------------------------------------------------------*
HEXA  *
HEXA  *----------------------------------------------------------------*
HEXA   9991-10-CHAMA-ROTINA-ABEND SECTION.
HEXA  *----------------------------------------------------------------*
HEXA  *
HEXA       MOVE    'GFCT0872'      TO          ERR-PGM.
HEXA  *
HEXA       CALL    WRK-ABEND-MESUX0 USING      WRK-BATCH-MESUX0
HEXA                                           ERRO-AREA.
HEXA  *
HEXA       GOBACK.
HEXA  *
HEXA  *----------------------------------------------------------------*
HEXA   9991-90-EXIT. EXIT.
HEXA  *----------------------------------------------------------------*
HEXA  *
