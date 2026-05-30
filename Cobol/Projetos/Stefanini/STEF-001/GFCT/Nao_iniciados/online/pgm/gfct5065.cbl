      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT5065.
       AUTHOR. MARTINS.
      *================================================================*
      *                   C P M   S I S T E M A S                      *
      *----------------------------------------------------------------*
      *                                                                *
      *    PROGRAMA     : GFCT5065                                     *
      *    PROGRAMADOR  : MARCELO RODRIGO MARTINS       - CPM/FPOLIS   *
      *    ANALISTA CPM : JOSE ZULIAM                   - CPM/FPOLIS   *
      *    ANALISTA     : RICARDO PEREIRA - PROCWORK    - GRUPO 50     *
      *    DATA         : 22/06/2005                                   *
      *                                                                *
      *    OBJETIVO     :                                              *
      *      GERAR DADOS PARA COMBO DE TARIFAS.                        *
      *                                                                *
      *    BANCO DE DADOS:                                             *
      *      DB2                                                       *
      *       TABLE                                INCLUDE/BOOK        *
      *       DB2PRD.SERVC_TARIF_PRINC               GFCTB0D8          *
      *       DB2PRD.PARM_SERVC_TARIF                GFCTB0A2          *
      *       DB2PRD.PARM_DATA_PROCM                 GFCTB0A1          *
      *                                                                *
      *    MODULOS CHAMADOS:                                           *
      *      GFCT5000 - OBTER DESCRICAO DE MENSAGENS                   *
      *      GFCT5500 - VERIFICACAO DE DISPONIBILIDADE ON-LINE.        *
      *      POOL0081 - ALOCAR DINAMICAMENTE UM MODULO                 *
      *      POOL1470 - CONSISTE DATAS COM SECULO                      *
      *                                                                *
      *    NAVEGACAO                                                   *
      *      CHAMADO POR: GFCT7081                                     *
      *                                                                *
      *================================================================*

      *================================================================*
       ENVIRONMENT                     DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
        CONFIGURATION                  SECTION.
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
       01  FILLER                      PIC X(32)           VALUE
           '*  INICIO DA WORKING GFCT5065  *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*         INDEXADORES          *'.
      *----------------------------------------------------------------*

       01  IND-1                       PIC 9(03)           VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*     VARIAVEIS AUXILIARES     *'.
      *----------------------------------------------------------------*

       01  WRK-FINALIZAR               PIC X(01)           VALUE SPACES.
       01  WRK-ERRO-DATA               PIC X(01)           VALUE SPACES.
       01  WRK-TIPO-VIGENCIA           PIC X(01)           VALUE SPACES.
       01  WRK-MODULO                  PIC X(08)           VALUE SPACES.
       01  WRK-NOME-TAB                PIC X(08)           VALUE SPACES.
       01  WRK-DT-DINIC                PIC 9(08)           VALUE ZEROS.
       01  WRK-DT-REFER                PIC 9(08)           VALUE ZEROS.
       01  WRK-DATA-PONTEIRO           PIC X(10)           VALUE SPACES.
       01  WRK-AUX-TARIFA              PIC 9(05)           VALUE ZEROS.
       01  WRK-NRO-SEQ                 PIC 9(04)           VALUE ZEROS.
       01  WRK-TAB                     PIC X(08)           VALUE SPACES.

       01  WRK-DATA-DD-MM-AAAA.
           03  WRK-DATA-DD             PIC 9(02)           VALUE ZEROS.
           03  FILLER                  PIC X(01)           VALUE '.'.
           03  WRK-DATA-MM             PIC 9(02)           VALUE ZEROS.
           03  FILLER                  PIC X(01)           VALUE '.'.
           03  WRK-DATA-AAAA           PIC 9(04)           VALUE ZEROS.

       01  WRK-DATA-REFER              PIC X(10)           VALUE SPACES.
       01  FILLER                      REDEFINES WRK-DATA-REFER.
           03  WRK-DTREF-DIA           PIC 9(02).
           03  WRK-P1                  PIC X(01).
           03  WRK-DTREF-MES           PIC 9(02).
           03  WRK-P2                  PIC X(01).
           03  WRK-DTREF-ANO           PIC 9(04).

       01  WRK-CAD-DATA.
           03  WRK-CAD-DIA             PIC 9(02)           VALUE ZEROS.
           03  WRK-CAD-MES             PIC 9(02)           VALUE ZEROS.
           03  WRK-CAD-ANO             PIC 9(04)           VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*     AREA DE MENSAGENS        *'.
      *----------------------------------------------------------------*

       01  WRK-MENSAGENS.
           03  WRK-MSG01               PIC X(75)           VALUE
               'ERRO NO SBAT3000 PARA OBTER A MENSAGEM DE ENTRADA'.
           03  WRK-MSG02               PIC X(75)           VALUE
               'ERRO NO SBAT3000 PARA ENVIAR MENSAGEM - MSGI'.
           03  WRK-MSG03               PIC X(75)           VALUE
               'ERRO NO SBAT3000 PARA ENVIAR MENSAGEM - MSGF'.
           03  WRK-MSG04               PIC X(75)           VALUE
               'ERRO NO SBAT3000 PARA ENVIAR MENSAGEM - RSTA'.
           03  WRK-MSG05.
               05  FILLER              PIC X(26)           VALUE
                   'ERRO NA CHAMADA DO MODULO'.
               05  WRK-MODULO-MSG05    PIC X(08)           VALUE SPACES.
               05  FILLER              PIC X(41)           VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*  AREA PARA MODULO GFCT5000   *'.
      *----------------------------------------------------------------*

       01  WRK-5000-AREA.
           03  WRK-5000-AREA-ENVIA.
               05  WRK-5000-AMBIENTE   PIC X(01)           VALUE SPACES.
               05  WRK-5000-COD-MSG    PIC 9(04)           VALUE ZEROS.
           03  WRK-5000-AREA-RECEBE.
               05  WRK-5000-COD-RETORNO
                                       PIC 9(02)           VALUE ZEROS.
               05  WRK-5000-COD-SQL-ERRO
                                       PIC 9(03)           VALUE ZEROS.
               05  WRK-5000-DESC-MSG   PIC X(70)           VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*  AREA PARA MODULO GFCT5500   *'.
      *----------------------------------------------------------------*

       01  WRK-5500-AREA-SAIDA.
           03  WRK-5500-COD-RETORNO    PIC  9(002)         VALUE ZEROS.
           03  WRK-5500-COD-SQL-ERRO   PIC  9(003)         VALUE ZEROS.
           03  WRK-5500-DESC-MSG       PIC  X(070)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*    AREA P/ MODULO GFCT5507   *'.
      *----------------------------------------------------------------*

       01  WRK-5507-AREA.
           03  WRK-5507-AREA-ENVIA.
               05  WRK-5507-AMBIENTE   PIC X(001)          VALUE SPACES.
               05  WRK-5507-CJUNC-DEPDC
                                       PIC 9(005)          VALUE ZEROS.
           03  WRK-5507-RECEBE.
               05  WRK-5507-COD-RETORNO
                                       PIC 9(002)          VALUE ZEROS.
               05  WRK-5507-COD-SQL-ERRO
                                       PIC 9(003)          VALUE ZEROS.
               05  WRK-5507-MSG-RETORNO
                                       PIC X(045)          VALUE SPACES.
               05  WRK-5507-IDTFD-MASTER
                                       PIC X(001)          VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*    AREA P/ MODULO SENH0315   *'.
      *----------------------------------------------------------------*

       01  WRK-VERSAO-0315             PIC  X(006)         VALUE
           'VRS001'.
       01  WRK-MENSAGEM-0315.
           03  WRK-COD-MENSAGEM-0315   PIC  9(003)         VALUE ZEROS.
           03  WRK-HIFEN-MENSAGEM-0315 PIC  X(001)         VALUE SPACES.
           03  WRK-TEXTO-MENSAGEM-0315 PIC  X(079)         VALUE SPACES.
       01  WRK-COD-FUNCIONAL-0315      PIC  X(007)         VALUE SPACES.
       01  WRK-NOME-FUNC-0315          PIC  X(040)         VALUE SPACES.
       01  WRK-COD-BANCO-0315          PIC  9(005)         VALUE ZEROS.
       01  WRK-NOME-BANCO-0315         PIC  X(040)         VALUE SPACES.
       01  WRK-CNPJ-BANCO-0315.
ST25X6*     03  WRK-CNPJ-PRI-0315       PIC  9(009)         VALUE ZEROS.
ST25X6*     03  WRK-CNPJ-FIL-0315       PIC  9(005)         VALUE ZEROS.
ST25X6*     03  WRK-CNPJ-CON-0315       PIC  9(002)         VALUE ZEROS.
           03  WRK-CNPJ-PRI-0315       PIC  X(009)         VALUE SPACES.
           03  WRK-CNPJ-FIL-0315       PIC  X(004)         VALUE SPACES.
           03  WRK-CNPJ-CON-0315       PIC  X(002)         VALUE SPACES.
       01  WRK-COD-JUNCAO-0315         PIC  9(005)         VALUE ZEROS.
       01  WRK-NOME-DEPTO-0315         PIC  X(040)         VALUE SPACES.
       01  WRK-SECAO-0315              PIC  9(003)         VALUE ZEROS.
       01  WRK-STATUS-0315             PIC  X(001)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*     AREAS DE TABELAS DB2     *'.
      *----------------------------------------------------------------*

           EXEC SQL
                INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB0D8
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB0A2
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB0A1
           END-EXEC.

           EXEC SQL
               DECLARE CSR-1 CURSOR FOR
               SELECT  DISTINCT
                       CSERVC_TARIF,
                       RSERVC_TARIF_REDZD,
                       CDEPDC
               FROM    DB2PRD.SERVC_TARIF_PRINC
               WHERE   CSERVC_TARIF      >= :GFCTB0D8.CSERVC-TARIF
               ORDER   BY CSERVC_TARIF
           END-EXEC.

           EXEC SQL
               DECLARE CSR-2 CURSOR FOR
               SELECT  DISTINCT
                       CSERVC_TARIF
               FROM    DB2PRD.PARM_SERVC_TARIF
               WHERE  (CSERVC_TARIF      >= :GFCTB0A2.CSERVC-TARIF)
                AND   (DINIC_VGCIA_TARIF <= :WRK-DATA-REFER
                AND    DFIM_VGCIA_TARIF  >= :WRK-DATA-REFER)
               ORDER   BY CSERVC_TARIF
           END-EXEC.

           EXEC SQL
               DECLARE CSR-3 CURSOR FOR
               SELECT  DISTINCT
                       CSERVC_TARIF
               FROM    DB2PRD.PARM_SERVC_TARIF
               WHERE  (CSERVC_TARIF      >= :GFCTB0A2.CSERVC-TARIF)
                AND   (DFIM_VGCIA_TARIF  <  :WRK-DATA-REFER
                OR     DINIC_VGCIA_TARIF >  :WRK-DATA-REFER)
               ORDER   BY CSERVC_TARIF
           END-EXEC.

           EXEC SQL
               DECLARE CSR-4 CURSOR FOR
               SELECT  CSERVC_TARIF,
                       DINIC_VGCIA_TARIF
               FROM    DB2PRD.PARM_SERVC_TARIF
               WHERE ((CSERVC_TARIF      >= :GFCTB0A2.CSERVC-TARIF
                AND    DINIC_VGCIA_TARIF
                                         >= :WRK-DATA-PONTEIRO)
                 OR    CSERVC_TARIF      >  :GFCTB0A2.CSERVC-TARIF)
                AND  ((DINIC_VGCIA_TARIF <= :WRK-DATA-REFER
                AND    DFIM_VGCIA_TARIF  >= :WRK-DATA-REFER)
                 OR   (DINIC_VGCIA_TARIF >  :WRK-DATA-REFER))
               ORDER  BY CSERVC_TARIF,
                         DINIC_VGCIA_TARIF
           END-EXEC.

           EXEC SQL
               DECLARE CSR-5 CURSOR FOR
               SELECT  DISTINCT
                       CSERVC_TARIF
               FROM    DB2PRD.PARM_SERVC_TARIF
               WHERE  (CSERVC_TARIF     >= :GFCTB0A2.CSERVC-TARIF)
                 AND  (DFIM_VGCIA_TARIF <  :WRK-DATA-REFER)
               ORDER   BY CSERVC_TARIF
           END-EXEC.

           EXEC SQL
               DECLARE CSR-6 CURSOR FOR
               SELECT  DISTINCT
                       CSERVC_TARIF
               FROM    DB2PRD.PARM_SERVC_TARIF
               WHERE  (CSERVC_TARIF      >= :GFCTB0A2.CSERVC-TARIF)
                 AND  (DINIC_VGCIA_TARIF >  :WRK-DATA-REFER)
               ORDER   BY CSERVC_TARIF
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)        VALUE
           '*    FIM DA WORKING GFCT5065   *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       COPY I#GFCTCI.
       COPY I#GFCTCJ.
       COPY I#GFCT0M.

      *================================================================*
       PROCEDURE DIVISION              USING GFCTCI-ENTRADA
                                             GFCTCJ-SAIDA
                                             GFCT0M-AREA-ERROS.
      *================================================================*

      *----------------------------------------------------------------*
       00000-ROTINA-PRINCIPAL          SECTION.
      *----------------------------------------------------------------*

           PERFORM 10000-INICIALIZAR.

           PERFORM 20000-PROCESSAR.

           PERFORM 30000-FINALIZAR.

      *----------------------------------------------------------------*
       00000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       10000-INICIALIZAR               SECTION.
      *----------------------------------------------------------------*

           PERFORM 11000-INICIALIZA-SAIDA.

           PERFORM 12000-CONSISTIR-DADOS.

           PERFORM 13000-VERIFICAR-ONLINE.

      *----------------------------------------------------------------*
       10000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       11000-INICIALIZA-SAIDA          SECTION.
      *----------------------------------------------------------------*

           INITIALIZE GFCTCJ-SAIDA.

           MOVE +268                   TO GFCTCJ-LL.
           MOVE ZEROS                  TO GFCTCJ-ZZ.
           MOVE GFCTCI-TRANSACAO       TO GFCTCJ-TRANSACAO.
           MOVE GFCTCI-FUNCAO          TO GFCTCJ-FUNCAO.

           IF  GFCTCI-QTDE-OCOR        NOT NUMERIC
               MOVE ZEROS              TO GFCTCJ-QTDE-OCOR
           ELSE
               MOVE GFCTCI-QTDE-OCOR   TO GFCTCJ-QTDE-OCOR
           END-IF.

           IF  GFCTCI-QTDE-TOT-REG     NOT NUMERIC
               MOVE ZEROS              TO GFCTCJ-QTDE-TOT-REG
           ELSE
               MOVE GFCTCI-QTDE-TOT-REG
                                       TO GFCTCJ-QTDE-TOT-REG
           END-IF

           MOVE GFCTCI-FUNC-BDSCO      TO GFCTCJ-FUNC-BDSCO.
           MOVE GFCTCI-TERMINAL        TO GFCTCJ-TERMINAL.
           MOVE GFCTCI-PONTEIRO-TARIFA
                                       TO GFCTCJ-PONTEIRO-TARIFA.
           MOVE GFCTCI-PONTEIRO-DATA   TO GFCTCJ-PONTEIRO-DATA.
           MOVE ZEROS                  TO GFCTCJ-ERRO
                                          GFCTCJ-COD-SQL-ERRO
                                          GFCTCJ-COD-MSG-ERRO
                                          WRK-5000-COD-MSG.
           MOVE 'N'                    TO GFCTCJ-FIM.

           PERFORM 11100-OBTER-DESC-MSG.

           MOVE WRK-5000-DESC-MSG      TO GFCTCJ-DESC-MSG-ERRO.

      *----------------------------------------------------------------*
       11000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       11100-OBTER-DESC-MSG            SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WRK-5000-AREA-RECEBE.

           MOVE 'O'                    TO WRK-5000-AMBIENTE.
           MOVE 'GFCT5000'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             WRK-5000-AREA-ENVIA
                                             WRK-5000-AREA-RECEBE.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 9                  TO GFCTCJ-ERRO

               MOVE 'GFCT5065'         TO GFCT0M-PROGRAMA
               MOVE 'GFCT5000'         TO WRK-MODULO-MSG05
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               PERFORM 30000-FINALIZAR
           END-IF.

           IF  WRK-5000-COD-RETORNO    NOT EQUAL ZEROS
               IF  WRK-5000-COD-RETORNO
                                       EQUAL 01
                   MOVE 1              TO GFCTCJ-ERRO
                   MOVE ZEROS          TO GFCTCJ-COD-SQL-ERRO
                                          GFCTCJ-COD-MSG-ERRO
                   MOVE WRK-5000-DESC-MSG
                                       TO GFCTCJ-DESC-MSG-ERRO
               END-IF
               IF  WRK-5000-COD-RETORNO
                                       EQUAL 99
                   MOVE 1              TO GFCTCJ-ERRO
                   MOVE WRK-5000-COD-SQL-ERRO
                                       TO GFCTCJ-COD-SQL-ERRO
                   MOVE ZEROS          TO GFCTCJ-COD-MSG-ERRO
                   MOVE WRK-5000-DESC-MSG
                                       TO GFCTCJ-DESC-MSG-ERRO
               END-IF
               MOVE 'S'                TO GFCTCJ-FIM
               INITIALIZE GFCTCJ-DADOS
               PERFORM 30000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       11100-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       12000-CONSISTIR-DADOS           SECTION.
      *----------------------------------------------------------------*

           IF (GFCTCI-TRANSACAO        EQUAL SPACES)          OR
              (GFCTCI-FUNCAO           EQUAL SPACES)          OR
              (GFCTCI-CSERVC-TARIF     IS NOT NUMERIC)        OR
              (GFCTCI-QTDE-OCOR        IS NOT NUMERIC)        OR
              (GFCTCI-QTDE-TOT-REG     IS NOT NUMERIC)        OR
              (GFCTCI-FUNC-BDSCO       EQUAL SPACES)          OR
              (GFCTCI-PONTEIRO-TARIFA  IS NOT NUMERIC)        OR
              (GFCTCI-TIPO-DATA        EQUAL  0               AND
               GFCTCI-PONTEIRO-DATA    EQUAL SPACES)          OR
              (GFCTCI-FIM              NOT EQUAL 'S' AND 'N') OR
              (GFCTCI-TIPO-VIGENCIA    NOT EQUAL 'V' AND 'N'  AND
                                                 'T' AND 'P'  AND
                                                 'H' AND 'F') OR
              (GFCTCI-TIPO-DATA        IS NOT NUMERIC         OR
               GFCTCI-TIPO-DATA        NOT EQUAL  0  AND  1   AND
                                                  2  AND  3 )
               MOVE 1                  TO GFCTCJ-ERRO
               MOVE ZEROS              TO GFCTCJ-COD-SQL-ERRO
               MOVE 0001               TO GFCTCJ-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'S'                TO GFCTCJ-FIM
               PERFORM 11100-OBTER-DESC-MSG
               MOVE WRK-5000-DESC-MSG  TO GFCTCJ-DESC-MSG-ERRO
               PERFORM 30000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       12000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       13000-VERIFICAR-ONLINE          SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT5500'             TO WRK-MODULO.
           INITIALIZE WRK-5500-AREA-SAIDA.

           CALL 'POOL0081'             USING WRK-MODULO
                                             WRK-5500-AREA-SAIDA.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 9                  TO GFCTCJ-ERRO

               MOVE 'GFCT5065'         TO GFCT0M-PROGRAMA
               MOVE 'GFCT5500'         TO WRK-MODULO-MSG05
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               PERFORM 30000-FINALIZAR
           END-IF.

           IF  WRK-5500-COD-RETORNO    EQUAL 04
               MOVE 1                  TO GFCTCJ-ERRO
               MOVE ZEROS              TO GFCTCJ-COD-SQL-ERRO
               MOVE 0030               TO GFCTCJ-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'S'                TO GFCTCJ-FIM
               PERFORM 11100-OBTER-DESC-MSG
               MOVE WRK-5000-DESC-MSG  TO GFCTCJ-DESC-MSG-ERRO
               PERFORM 30000-FINALIZAR
           ELSE
               IF  WRK-5500-COD-RETORNO
                                       EQUAL 99
                   INITIALIZE GFCT0M-ERRO-SQL
                   MOVE 'PARM_CTRL_GSTAO '
                                       TO GFCT0M-NOME-TAB
                   MOVE 'SELECT'       TO GFCT0M-COMANDO-SQL
                   MOVE  WRK-5500-COD-SQL-ERRO
                                       TO GFCT0M-SQLCODE
                                          GFCTCJ-COD-SQL-ERRO
                   MOVE '0019'         TO GFCT0M-LOCAL
                   MOVE 0012           TO GFCTCJ-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
                   MOVE 'GFCTB0A0'     TO WRK-NOME-TAB
                   PERFORM 23210-MOVER-ERRO-SQL
                   PERFORM 30000-FINALIZAR
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       13000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       20000-PROCESSAR                 SECTION.
      *----------------------------------------------------------------*

           PERFORM 21000-OBTER-COD-JUNCAO.

           PERFORM 22000-VERIFICAR-MASTER.

           PERFORM 23000-FORMATAR-DT-REF

           IF  GFCTCI-PONTEIRO-TARIFA  EQUAL ZEROS
               MOVE GFCTCI-CSERVC-TARIF
                                       TO WRK-AUX-TARIFA
               MOVE '01.01.0001'       TO WRK-DATA-PONTEIRO
           ELSE
               MOVE GFCTCI-PONTEIRO-TARIFA
                                       TO WRK-AUX-TARIFA
               MOVE GFCTCI-PONTEIRO-DATA
                                       TO WRK-DATA-PONTEIRO
           END-IF.

           EVALUATE GFCTCI-TIPO-VIGENCIA
               WHEN 'T'
                   MOVE 235            TO WRK-NRO-SEQ
                   MOVE 'GFCTB0D8'     TO WRK-TAB
                   PERFORM 24000-TRATAR-CSR-1
               WHEN 'V'
                   MOVE 236            TO WRK-NRO-SEQ
                   MOVE 'GFCTB0A2'     TO WRK-TAB
                   PERFORM 25000-TRATAR-CSR-2
               WHEN 'N'
                   MOVE 237            TO WRK-NRO-SEQ
                   MOVE 'GFCTB0A2'     TO WRK-TAB
                   PERFORM 26000-TRATAR-CSR-3
               WHEN 'P'
                   MOVE 238            TO WRK-NRO-SEQ
                   MOVE 'GFCTB0A2'     TO WRK-TAB
                   PERFORM 27000-TRATAR-CSR-4
               WHEN 'H'
                   MOVE 239            TO WRK-NRO-SEQ
                   MOVE 'GFCTB0A2'     TO WRK-TAB
                   PERFORM 28000-TRATAR-CSR-5
               WHEN OTHER
                   MOVE 240            TO WRK-NRO-SEQ
                   MOVE 'GFCTB0A2'     TO WRK-TAB
                   PERFORM 29000-TRATAR-CSR-6
           END-EVALUATE.

      *----------------------------------------------------------------*
       20000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       21000-OBTER-COD-JUNCAO          SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                     TO WRK-VERSAO-0315
                                              WRK-COD-FUNCIONAL-0315
                                              WRK-NOME-FUNC-0315
                                              WRK-NOME-BANCO-0315
                                              WRK-NOME-DEPTO-0315
                                              WRK-STATUS-0315
                                              WRK-HIFEN-MENSAGEM-0315
                                              WRK-TEXTO-MENSAGEM-0315.
           MOVE ZEROS                      TO WRK-COD-JUNCAO-0315
                                              WRK-COD-BANCO-0315
                                              WRK-SECAO-0315
                                              WRK-COD-MENSAGEM-0315
                                              WRK-CNPJ-PRI-0315
                                              WRK-CNPJ-FIL-0315
                                              WRK-CNPJ-CON-0315.
           MOVE 'VRS001'                   TO WRK-VERSAO-0315.
           MOVE GFCTCI-FUNC-BDSCO          TO WRK-COD-FUNCIONAL-0315.

           CALL 'SENH0315'                 USING WRK-VERSAO-0315
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

           IF  RETURN-CODE                 NOT EQUAL ZEROS AND 04
               MOVE 'S'                    TO GFCTCJ-FIM
               MOVE 1                      TO GFCTCJ-ERRO
               MOVE RETURN-CODE            TO GFCTCJ-COD-SQL-ERRO
               MOVE 0056                   TO GFCTCJ-COD-MSG-ERRO
                                              WRK-5000-COD-MSG
               PERFORM 11100-OBTER-DESC-MSG
               MOVE WRK-5000-DESC-MSG      TO GFCTCJ-DESC-MSG-ERRO
               PERFORM 30000-FINALIZAR
           END-IF.

           IF  RETURN-CODE                 EQUAL 04
               MOVE 'S'                    TO GFCTCJ-FIM
               IF  WRK-COD-MENSAGEM-0315   EQUAL 001
                   MOVE 1                  TO GFCTCJ-ERRO
                   MOVE ZEROS              TO GFCTCJ-COD-SQL-ERRO
                   MOVE 0057               TO GFCTCJ-COD-MSG-ERRO
                                              WRK-5000-COD-MSG
                   PERFORM 11100-OBTER-DESC-MSG
                   MOVE WRK-5000-DESC-MSG  TO GFCTCJ-DESC-MSG-ERRO
                   PERFORM 30000-FINALIZAR
               END-IF

               IF  WRK-COD-MENSAGEM-0315   EQUAL 154
                   MOVE 1                  TO GFCTCJ-ERRO
                   MOVE ZEROS              TO GFCTCJ-COD-SQL-ERRO
                   MOVE 0058               TO GFCTCJ-COD-MSG-ERRO
                                              WRK-5000-COD-MSG
                   PERFORM 11100-OBTER-DESC-MSG
                   MOVE WRK-5000-DESC-MSG  TO GFCTCJ-DESC-MSG-ERRO
                   PERFORM 30000-FINALIZAR
               END-IF

               IF  WRK-COD-MENSAGEM-0315   EQUAL 242
                   MOVE 1                  TO GFCTCJ-ERRO
                   MOVE ZEROS              TO GFCTCJ-COD-SQL-ERRO
                   MOVE 0059               TO GFCTCJ-COD-MSG-ERRO
                                              WRK-5000-COD-MSG
                   PERFORM 11100-OBTER-DESC-MSG
                   MOVE WRK-5000-DESC-MSG  TO GFCTCJ-DESC-MSG-ERRO
                   PERFORM 30000-FINALIZAR
               END-IF

               IF  WRK-COD-MENSAGEM-0315   EQUAL 009
                   MOVE ZEROS              TO GFCTCJ-ERRO
                                              GFCTCJ-COD-SQL-ERRO
                                              GFCTCJ-COD-MSG-ERRO
                                              WRK-5000-COD-MSG
                   PERFORM 11100-OBTER-DESC-MSG
                   MOVE WRK-5000-DESC-MSG  TO GFCTCJ-DESC-MSG-ERRO
                   PERFORM 30000-FINALIZAR
               END-IF

               IF  WRK-COD-MENSAGEM-0315   EQUAL 138
                   MOVE 1                  TO GFCTCJ-ERRO
                   MOVE ZEROS              TO GFCTCJ-COD-SQL-ERRO
                   MOVE 0060               TO GFCTCJ-COD-MSG-ERRO
                                              WRK-5000-COD-MSG
                   PERFORM 11100-OBTER-DESC-MSG
                   MOVE WRK-5000-DESC-MSG  TO GFCTCJ-DESC-MSG-ERRO
                   PERFORM 30000-FINALIZAR
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       21000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       22000-VERIFICAR-MASTER          SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                  WRK-5507-AREA.

           MOVE 'O'                    TO WRK-5507-AMBIENTE.
           MOVE WRK-COD-JUNCAO-0315    TO WRK-5507-CJUNC-DEPDC.
           MOVE 'GFCT5507'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             WRK-5507-AREA.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 9                  TO GFCTCJ-ERRO

               MOVE 'GFCT5065'         TO GFCT0M-PROGRAMA
               MOVE 'GFCT5507'         TO WRK-MODULO-MSG05
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               PERFORM 30000-FINALIZAR
           END-IF.

           IF  WRK-5507-COD-RETORNO    NOT EQUAL ZEROS
               MOVE 'S'                    TO GFCTCJ-FIM
               IF  WRK-5507-COD-RETORNO
                                       EQUAL 01
                   MOVE 1              TO GFCTCJ-ERRO
                   MOVE ZEROS          TO GFCTCJ-COD-SQL-ERRO
                                          GFCTCJ-COD-MSG-ERRO
                   MOVE WRK-5507-MSG-RETORNO
                                       TO GFCTCJ-DESC-MSG-ERRO
               ELSE
                   IF  WRK-5507-COD-RETORNO
                                       EQUAL 99
                       MOVE 1          TO GFCTCJ-ERRO
                       MOVE WRK-5507-COD-SQL-ERRO
                                       TO GFCTCJ-COD-SQL-ERRO
                       MOVE ZEROS      TO GFCTCJ-COD-MSG-ERRO
                       MOVE WRK-5507-MSG-RETORNO
                                       TO GFCTCJ-DESC-MSG-ERRO
                   END-IF
               END-IF
               MOVE SPACES             TO GFCTCJ-DADOS
               INITIALIZE GFCTCJ-DADOS
               PERFORM 30000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       22000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       23000-FORMATAR-DT-REF           SECTION.
      *----------------------------------------------------------------*

           IF  GFCTCI-TIPO-DATA        EQUAL ZEROS
               MOVE GFCTCI-DATA-REFER  TO WRK-DATA-DD-MM-AAAA
               PERFORM 23100-FORMATAR-DT-WEB
               IF  WRK-ERRO-DATA       EQUAL 'S'
                   MOVE 1              TO GFCTCJ-ERRO
                   MOVE ZEROS          TO GFCTCJ-COD-SQL-ERRO
                   MOVE 125            TO GFCTCJ-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
                   MOVE 'S'            TO GFCTCJ-FIM
                   PERFORM 11100-OBTER-DESC-MSG
                   MOVE WRK-5000-DESC-MSG
                                       TO GFCTCJ-DESC-MSG-ERRO
                   PERFORM 30000-FINALIZAR
               ELSE
                   MOVE WRK-DATA-AAAA  TO WRK-DTREF-ANO
                   MOVE WRK-DATA-MM    TO WRK-DTREF-MES
                   MOVE WRK-DATA-DD    TO WRK-DTREF-DIA
                   MOVE '.'            TO WRK-P1
                                          WRK-P2
               END-IF
           ELSE
               PERFORM 23200-OBTER-DT-PROCE
               IF  GFCTCI-TIPO-DATA    EQUAL 1
                   MOVE DPROCM-ANTER OF GFCTB0A1
                                       TO WRK-DATA-REFER
               ELSE
                   IF  GFCTCI-TIPO-DATA
                                       EQUAL 2
                       MOVE DPROCM-ATUAL OF GFCTB0A1
                                       TO WRK-DATA-REFER
                   ELSE
                       MOVE DPROCM-PROX OF GFCTB0A1
                                       TO WRK-DATA-REFER
                   END-IF
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       23000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       23100-FORMATAR-DT-WEB           SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                    TO WRK-ERRO-DATA.

           IF  WRK-DATA-DD-MM-AAAA     EQUAL SPACES
               MOVE 'S'                TO WRK-ERRO-DATA
           ELSE
               MOVE WRK-DATA-DD        TO WRK-CAD-DIA
               MOVE WRK-DATA-MM        TO WRK-CAD-MES
               MOVE WRK-DATA-AAAA      TO WRK-CAD-ANO
               CALL 'POOL1470'         USING WRK-CAD-DATA
               IF  RETURN-CODE         NOT EQUAL ZEROS
                   MOVE 'S'            TO WRK-ERRO-DATA
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       23100-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       23200-OBTER-DT-PROCE            SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT'                 TO CSIST-PRINC OF GFCTB0A1.

           EXEC SQL
               SELECT  DPROCM_ANTER,
                       DPROCM_ATUAL,
                       DPROCM_PROX
               INTO    :GFCTB0A1.DPROCM-ANTER,
                       :GFCTB0A1.DPROCM-ATUAL,
                       :GFCTB0A1.DPROCM-PROX
               FROM    DB2PRD.PARM_DATA_PROCM
               WHERE   CSIST_PRINC     = :GFCTB0A1.CSIST-PRINC
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'PARM_DATA_PROCM'  TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTCJ-COD-SQL-ERRO
               MOVE '0001'             TO GFCT0M-LOCAL
               MOVE 0011               TO GFCTCJ-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB0A1'         TO WRK-NOME-TAB
               PERFORM 23210-MOVER-ERRO-SQL
               PERFORM 30000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       23200-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       23210-MOVER-ERRO-SQL            SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT5065'             TO GFCT0M-PROGRAMA.
           MOVE 'DB2'                  TO GFCT0M-TIPO-ACESSO.
           MOVE SQLCA                  TO GFCT0M-SQLCA-AREA.
           MOVE 9                      TO GFCTCJ-ERRO.

           PERFORM 11100-OBTER-DESC-MSG.

           PERFORM 23211-CONCATENAR-MSG.

      *----------------------------------------------------------------*
       23210-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       23211-CONCATENAR-MSG            SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTCJ-DESC-MSG-ERRO.

           STRING WRK-5000-DESC-MSG ' - '
                  WRK-NOME-TAB
           DELIMITED BY '   '          INTO GFCTCJ-DESC-MSG-ERRO.

      *----------------------------------------------------------------*
       23211-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       24000-TRATAR-CSR-1              SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                    TO WRK-FINALIZAR.

           PERFORM 24100-ABRIR-CSR-1.

           PERFORM 24200-LER-CSR-1.

           PERFORM VARYING IND-1       FROM 1 BY 1
                   UNTIL   IND-1       GREATER 7 OR
                         WRK-FINALIZAR EQUAL 'S'
               MOVE 'T'                TO WRK-TIPO-VIGENCIA
               PERFORM 25300-MOVER-CAMPOS-TARIFA
               PERFORM 24200-LER-CSR-1
           END-PERFORM.

           IF  WRK-FINALIZAR           NOT EQUAL 'S'
               MOVE CSERVC-TARIF OF GFCTB0D8
                                       TO GFCTCJ-PONTEIRO-TARIFA
           END-IF.

           PERFORM 24400-FECHAR-CSR-1.

      *----------------------------------------------------------------*
       24000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       24100-ABRIR-CSR-1               SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-AUX-TARIFA         TO CSERVC-TARIF     OF GFCTB0D8.

           EXEC SQL
               OPEN CSR-1
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'SERVC_TARIF_PRINC'
                                       TO GFCT0M-NOME-TAB
               MOVE 'OPEN'             TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTCJ-COD-SQL-ERRO
               MOVE '0003'             TO GFCT0M-LOCAL
               MOVE 0011               TO GFCTCJ-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB0B8'         TO WRK-NOME-TAB
               PERFORM 23210-MOVER-ERRO-SQL
               PERFORM 30000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       24100-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       24200-LER-CSR-1                 SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               FETCH   CSR-1
               INTO    :GFCTB0D8.CSERVC-TARIF,
                       :GFCTB0D8.RSERVC-TARIF-REDZD,
                       :GFCTB0D8.CDEPDC
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'SERVC_TARIF_PRINC'
                                       TO GFCT0M-NOME-TAB
               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTCJ-COD-SQL-ERRO
               MOVE '0002'             TO GFCT0M-LOCAL
               MOVE 0006               TO GFCTCJ-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB0D8'         TO WRK-NOME-TAB
               PERFORM 23210-MOVER-ERRO-SQL
               PERFORM 30000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               PERFORM 24210-TRATAR-FIM-CSR
           ELSE
               IF (WRK-5507-IDTFD-MASTER
                                       EQUAL 'N' AND
                   WRK-COD-JUNCAO-0315 NOT EQUAL CDEPDC OF GFCTB0D8)
                   GO                  TO 24200-LER-CSR-1
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       24200-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       24210-TRATAR-FIM-CSR            SECTION.
      *----------------------------------------------------------------*

           MOVE 'S'                    TO WRK-FINALIZAR
                                          GFCTCJ-FIM.

           IF  IND-1                   EQUAL ZEROS
               MOVE ZEROS              TO GFCTCJ-ERRO
                                          GFCTCJ-COD-SQL-ERRO
               MOVE WRK-NRO-SEQ        TO GFCTCJ-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE WRK-TAB            TO WRK-NOME-TAB
               PERFORM 11100-OBTER-DESC-MSG
               PERFORM 23211-CONCATENAR-MSG
           END-IF.

      *----------------------------------------------------------------*
       24210-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       24400-FECHAR-CSR-1              SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               CLOSE CSR-1
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'SERVC_TARIF_PRINC'
                                       TO GFCT0M-NOME-TAB
               MOVE 'CLOSE'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTCJ-COD-SQL-ERRO
               MOVE '0003'             TO GFCT0M-LOCAL
               MOVE 0011               TO GFCTCJ-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB0D8'         TO WRK-NOME-TAB
               PERFORM 23210-MOVER-ERRO-SQL
           END-IF.

      *----------------------------------------------------------------*
       24400-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       25000-TRATAR-CSR-2              SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                    TO WRK-FINALIZAR.

           PERFORM 25100-ABRIR-CSR-2.

           PERFORM 25200-LER-CSR-2.

           PERFORM                     VARYING IND-1 FROM 1 BY 1
                                       UNTIL IND-1 GREATER 7 OR
                                       WRK-FINALIZAR EQUAL 'S'
               MOVE 'V'                TO WRK-TIPO-VIGENCIA
               PERFORM 25300-MOVER-CAMPOS-TARIFA
               PERFORM 25200-LER-CSR-2
           END-PERFORM.

           IF  WRK-FINALIZAR           NOT EQUAL 'S'
               MOVE CSERVC-TARIF OF GFCTB0A2
                                       TO GFCTCJ-PONTEIRO-TARIFA
           END-IF.

           PERFORM 25400-FECHAR-CSR-2.

      *----------------------------------------------------------------*
       25000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       25100-ABRIR-CSR-2               SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-AUX-TARIFA         TO CSERVC-TARIF      OF GFCTB0A2.

           EXEC SQL
               OPEN CSR-2
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'PARM_SERVC_TARIF' TO GFCT0M-NOME-TAB
               MOVE 'OPEN'             TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTCJ-COD-SQL-ERRO
               MOVE '0007'             TO GFCT0M-LOCAL
               MOVE 0005               TO GFCTCJ-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB0A2'         TO WRK-NOME-TAB
               PERFORM 23210-MOVER-ERRO-SQL
               PERFORM 30000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       25100-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       25200-LER-CSR-2                 SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               FETCH  CSR-2
               INTO   :GFCTB0A2.CSERVC-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'PARM_SERVC_TARIF' TO GFCT0M-NOME-TAB
               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTCJ-COD-SQL-ERRO
               MOVE '0005'             TO GFCT0M-LOCAL
               MOVE 0006               TO GFCTCJ-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB0A2'         TO WRK-NOME-TAB
               PERFORM 23210-MOVER-ERRO-SQL
               PERFORM 30000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               PERFORM 24210-TRATAR-FIM-CSR
           ELSE
               PERFORM 25310-OBTER-DESC-TARIFA
               IF (WRK-5507-IDTFD-MASTER
                                       EQUAL 'N' AND
                   WRK-COD-JUNCAO-0315 NOT EQUAL CDEPDC OF GFCTB0D8)
                   GO                  TO 25200-LER-CSR-2
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       25200-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       25300-MOVER-CAMPOS-TARIFA       SECTION.
      *----------------------------------------------------------------*

           ADD 106                     TO GFCTCJ-LL

           MOVE CSERVC-TARIF OF GFCTB0D8
                                       TO GFCTCJ-COD-TARIFA(IND-1).
           MOVE RSERVC-TARIF-REDZD OF GFCTB0D8
                                       TO GFCTCJ-DESC-TARIFA(IND-1).
           MOVE WRK-TIPO-VIGENCIA      TO GFCTCJ-TIPO-VIGENCIA(IND-1).

           ADD 1                       TO GFCTCJ-QTDE-TOT-REG.

      *----------------------------------------------------------------*
       25300-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       25310-OBTER-DESC-TARIFA         SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF OF GFCTB0A2
                                       TO CSERVC-TARIF OF GFCTB0D8.

           EXEC SQL
               SELECT  CDEPDC,
                       CSERVC_TARIF,
                       RSERVC_TARIF_REDZD
               INTO    :GFCTB0D8.CDEPDC,
                       :GFCTB0D8.CSERVC-TARIF,
                       :GFCTB0D8.RSERVC-TARIF-REDZD
               FROM    DB2PRD.SERVC_TARIF_PRINC
               WHERE   CSERVC_TARIF     = :GFCTB0D8.CSERVC-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100 AND
                                                 -811) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'SERVC_TARIF_PRINC'
                                       TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTCJ-COD-SQL-ERRO
               MOVE '0004'             TO GFCT0M-LOCAL
               MOVE 0009               TO GFCTCJ-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB0D8'         TO WRK-NOME-TAB
               PERFORM 23210-MOVER-ERRO-SQL
               PERFORM 30000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FINALIZAR
                                          GFCTCJ-FIM
               IF  IND-1               EQUAL ZEROS
                   MOVE 1              TO GFCTCJ-ERRO
                   MOVE ZEROS          TO GFCTCJ-COD-SQL-ERRO
                   MOVE 0242           TO GFCTCJ-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
                   PERFORM 11100-OBTER-DESC-MSG
                   MOVE WRK-5000-DESC-MSG  TO GFCTCJ-DESC-MSG-ERRO
                   PERFORM 30000-FINALIZAR
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       25310-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       25400-FECHAR-CSR-2              SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               CLOSE CSR-2
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'PARM_SERVC_TARIF' TO GFCT0M-NOME-TAB
               MOVE 'CLOSE'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTCJ-COD-SQL-ERRO
               MOVE '0006'             TO GFCT0M-LOCAL
               MOVE 0011               TO GFCTCJ-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB0A2'         TO WRK-NOME-TAB
               PERFORM 23210-MOVER-ERRO-SQL
           END-IF.

      *----------------------------------------------------------------*
       25400-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       26000-TRATAR-CSR-3              SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                    TO WRK-FINALIZAR.

           PERFORM 26100-ABRIR-CSR-3.

           PERFORM 26200-LER-CSR-3.

           PERFORM                     VARYING IND-1 FROM 1 BY 1
                                       UNTIL IND-1 GREATER 7 OR
                                       WRK-FINALIZAR EQUAL 'S'
               MOVE ' '                TO WRK-TIPO-VIGENCIA
               PERFORM 25300-MOVER-CAMPOS-TARIFA
               PERFORM 26200-LER-CSR-3
           END-PERFORM.

           IF  WRK-FINALIZAR           NOT EQUAL 'S'
               MOVE CSERVC-TARIF OF GFCTB0A2
                                       TO GFCTCJ-PONTEIRO-TARIFA
           END-IF.

           PERFORM 26300-FECHAR-CSR-3.

      *----------------------------------------------------------------*
       26000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       26100-ABRIR-CSR-3               SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-AUX-TARIFA         TO CSERVC-TARIF      OF GFCTB0A2.

           EXEC SQL
               OPEN CSR-3
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'PARM_SERVC_TARIF' TO GFCT0M-NOME-TAB
               MOVE 'OPEN'             TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTCJ-COD-SQL-ERRO
               MOVE '0007'             TO GFCT0M-LOCAL
               MOVE 0005               TO GFCTCJ-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB0A2'         TO WRK-NOME-TAB
               PERFORM 23210-MOVER-ERRO-SQL
               PERFORM 30000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       26100-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       26200-LER-CSR-3                 SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               FETCH CSR-3
               INTO  :GFCTB0A2.CSERVC-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'PARM_SERVC_TARIF' TO GFCT0M-NOME-TAB
               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTCJ-COD-SQL-ERRO
               MOVE '0008'             TO GFCT0M-LOCAL
               MOVE 0006               TO GFCTCJ-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB0A2'         TO WRK-NOME-TAB
               PERFORM 23210-MOVER-ERRO-SQL
               PERFORM 30000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               PERFORM 24210-TRATAR-FIM-CSR
           ELSE
               PERFORM 25310-OBTER-DESC-TARIFA
               IF (WRK-5507-IDTFD-MASTER
                                       EQUAL 'N' AND
                   WRK-COD-JUNCAO-0315 NOT EQUAL CDEPDC OF GFCTB0D8)
                   GO                  TO 26200-LER-CSR-3
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       26200-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       26300-FECHAR-CSR-3              SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               CLOSE CSR-3
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'PARM_SERVC_TARIF' TO GFCT0M-NOME-TAB
               MOVE 'CLOSE'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTCJ-COD-SQL-ERRO
               MOVE '0009'             TO GFCT0M-LOCAL
               MOVE 0011               TO GFCTCJ-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB0A2'         TO WRK-NOME-TAB
               PERFORM 23210-MOVER-ERRO-SQL
           END-IF.

      *----------------------------------------------------------------*
       26300-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       27000-TRATAR-CSR-4              SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                    TO WRK-FINALIZAR.

           PERFORM 27100-ABRIR-CSR-4.

           PERFORM 27200-LER-CSR-4.

           PERFORM                     VARYING IND-1 FROM 1 BY 1
                                       UNTIL IND-1 GREATER 7 OR
                                       WRK-FINALIZAR EQUAL 'S'
               PERFORM 27300-INVERTER-DATAS
               IF  WRK-DT-DINIC        GREATER WRK-DT-REFER
                   MOVE 'P'            TO WRK-TIPO-VIGENCIA
               ELSE
                   MOVE 'V'            TO WRK-TIPO-VIGENCIA
               END-IF
               PERFORM 25300-MOVER-CAMPOS-TARIFA
               PERFORM 27200-LER-CSR-4
           END-PERFORM.

           IF  WRK-FINALIZAR           NOT EQUAL 'S'
               MOVE CSERVC-TARIF OF GFCTB0A2
                                       TO GFCTCJ-PONTEIRO-TARIFA
               MOVE DINIC-VGCIA-TARIF OF GFCTB0A2
                                       TO GFCTCJ-PONTEIRO-DATA
           END-IF.

           PERFORM 27400-FECHAR-CSR-4.

      *----------------------------------------------------------------*
       27000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       27100-ABRIR-CSR-4               SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-AUX-TARIFA         TO CSERVC-TARIF     OF GFCTB0A2.

           EXEC SQL
               OPEN CSR-4
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'PARM_SERVC_TARIF' TO GFCT0M-NOME-TAB
               MOVE 'OPEN'             TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTCJ-COD-SQL-ERRO
               MOVE '0010'             TO GFCT0M-LOCAL
               MOVE 0005               TO GFCTCJ-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB0A2'         TO WRK-NOME-TAB
               PERFORM 23210-MOVER-ERRO-SQL
               PERFORM 30000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       27100-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       27200-LER-CSR-4                 SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               FETCH CSR-4
               INTO :GFCTB0A2.CSERVC-TARIF,
                    :GFCTB0A2.DINIC-VGCIA-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'PARM_SERVC_TARIF' TO GFCT0M-NOME-TAB
               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTCJ-COD-SQL-ERRO
               MOVE '0011'             TO GFCT0M-LOCAL
               MOVE 0006               TO GFCTCJ-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB0A2'         TO WRK-NOME-TAB
               PERFORM 23210-MOVER-ERRO-SQL
               PERFORM 30000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               PERFORM 24210-TRATAR-FIM-CSR
           ELSE
               PERFORM 25310-OBTER-DESC-TARIFA
               IF (WRK-5507-IDTFD-MASTER
                                       EQUAL 'N' AND
                   WRK-COD-JUNCAO-0315 NOT EQUAL CDEPDC OF GFCTB0D8)
                   GO                  TO 27200-LER-CSR-4
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       27200-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       27300-INVERTER-DATAS            SECTION.
      *----------------------------------------------------------------*

           STRING DINIC-VGCIA-TARIF OF GFCTB0A2 (07:04)
                  DINIC-VGCIA-TARIF OF GFCTB0A2 (04:02)
                  DINIC-VGCIA-TARIF OF GFCTB0A2 (01:02)
           DELIMITED BY SIZE           INTO WRK-DT-DINIC.

           STRING WRK-DTREF-ANO
                  WRK-DTREF-MES
                  WRK-DTREF-DIA
           DELIMITED BY SIZE           INTO WRK-DT-REFER.

      *----------------------------------------------------------------*
       27300-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       27400-FECHAR-CSR-4              SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               CLOSE CSR-4
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'PARM_SERVC_TARIF' TO GFCT0M-NOME-TAB
               MOVE 'CLOSE'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTCJ-COD-SQL-ERRO
               MOVE '0012'             TO GFCT0M-LOCAL
               MOVE 0011               TO GFCTCJ-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB0A2'         TO WRK-NOME-TAB
               PERFORM 23210-MOVER-ERRO-SQL
           END-IF.

      *----------------------------------------------------------------*
       27400-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       28000-TRATAR-CSR-5              SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                    TO WRK-FINALIZAR.

           PERFORM 28100-ABRIR-CSR-5.

           PERFORM 28200-LER-CSR-5.

           PERFORM                     VARYING IND-1 FROM 1 BY 1
                                       UNTIL IND-1 GREATER 7 OR
                                       WRK-FINALIZAR EQUAL 'S'
               MOVE 'H'                TO WRK-TIPO-VIGENCIA
               PERFORM 25300-MOVER-CAMPOS-TARIFA
               PERFORM 28200-LER-CSR-5
           END-PERFORM.

           IF  WRK-FINALIZAR           NOT EQUAL 'S'
               MOVE CSERVC-TARIF OF GFCTB0A2
                                       TO GFCTCJ-PONTEIRO-TARIFA
           END-IF.

           PERFORM 28300-FECHAR-CSR-5.

      *----------------------------------------------------------------*
       28000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       28100-ABRIR-CSR-5               SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-AUX-TARIFA         TO CSERVC-TARIF     OF GFCTB0A2.

           EXEC SQL
               OPEN CSR-5
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'PARM_SERVC_TARIF' TO GFCT0M-NOME-TAB
               MOVE 'OPEN'             TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTCJ-COD-SQL-ERRO
               MOVE '0013'             TO GFCT0M-LOCAL
               MOVE 0005               TO GFCTCJ-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB0A2'         TO WRK-NOME-TAB
               PERFORM 23210-MOVER-ERRO-SQL
               PERFORM 30000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       28100-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       28200-LER-CSR-5                 SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               FETCH CSR-5
               INTO  :GFCTB0A2.CSERVC-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'PARM_SERVC_TARIF' TO GFCT0M-NOME-TAB
               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTCJ-COD-SQL-ERRO
               MOVE '0014'             TO GFCT0M-LOCAL
               MOVE 0006               TO GFCTCJ-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB0A2'         TO WRK-NOME-TAB
               PERFORM 23210-MOVER-ERRO-SQL
               PERFORM 30000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               PERFORM 24210-TRATAR-FIM-CSR
           ELSE
               PERFORM 25310-OBTER-DESC-TARIFA
               IF (WRK-5507-IDTFD-MASTER
                                       EQUAL 'N' AND
                   WRK-COD-JUNCAO-0315 NOT EQUAL CDEPDC OF GFCTB0D8)
                   GO                  TO 28200-LER-CSR-5
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       28200-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       28300-FECHAR-CSR-5              SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               CLOSE CSR-5
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'PARM_SERVC_TARIF' TO GFCT0M-NOME-TAB
               MOVE 'CLOSE'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTCJ-COD-SQL-ERRO
               MOVE '0015'             TO GFCT0M-LOCAL
               MOVE 0011               TO GFCTCJ-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB0A2'         TO WRK-NOME-TAB
               PERFORM 23210-MOVER-ERRO-SQL
           END-IF.

      *----------------------------------------------------------------*
       28300-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       29000-TRATAR-CSR-6              SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                    TO WRK-FINALIZAR.

           PERFORM 29100-ABRIR-CSR-6.

           PERFORM 29200-LER-CSR-6.

           PERFORM                     VARYING IND-1 FROM 1 BY 1
                                       UNTIL IND-1 GREATER 7 OR
                                       WRK-FINALIZAR EQUAL 'S'
               MOVE 'F'                TO WRK-TIPO-VIGENCIA
               PERFORM 25300-MOVER-CAMPOS-TARIFA
               PERFORM 29200-LER-CSR-6
           END-PERFORM.

           IF  WRK-FINALIZAR           NOT EQUAL 'S'
               MOVE CSERVC-TARIF OF GFCTB0A2
                                       TO GFCTCJ-PONTEIRO-TARIFA
           END-IF.

           PERFORM 29300-FECHAR-CSR-6.

      *----------------------------------------------------------------*
       29000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       29100-ABRIR-CSR-6               SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-AUX-TARIFA         TO CSERVC-TARIF     OF GFCTB0A2.

           EXEC SQL
               OPEN CSR-6
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'PARM_SERVC_TARIF' TO GFCT0M-NOME-TAB
               MOVE 'OPEN'             TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTCJ-COD-SQL-ERRO
               MOVE '0016'             TO GFCT0M-LOCAL
               MOVE 0005               TO GFCTCJ-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB0A2'         TO WRK-NOME-TAB
               PERFORM 23210-MOVER-ERRO-SQL
               PERFORM 30000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       29100-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       29200-LER-CSR-6                 SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               FETCH CSR-6
               INTO  :GFCTB0A2.CSERVC-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'PARM_SERVC_TARIF' TO GFCT0M-NOME-TAB
               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTCJ-COD-SQL-ERRO
               MOVE '0017'             TO GFCT0M-LOCAL
               MOVE 0006               TO GFCTCJ-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB0A2'         TO WRK-NOME-TAB
               PERFORM 23210-MOVER-ERRO-SQL
               PERFORM 30000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               PERFORM 24210-TRATAR-FIM-CSR
           ELSE
               PERFORM 25310-OBTER-DESC-TARIFA
               IF (WRK-5507-IDTFD-MASTER
                                       EQUAL 'N' AND
                   WRK-COD-JUNCAO-0315 NOT EQUAL CDEPDC OF GFCTB0D8)
                   GO                  TO 29200-LER-CSR-6
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       29200-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       29300-FECHAR-CSR-6              SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               CLOSE CSR-6
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'PARM_SERVC_TARIF' TO GFCT0M-NOME-TAB
               MOVE 'CLOSE'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          GFCTCJ-COD-SQL-ERRO
               MOVE '0018'             TO GFCT0M-LOCAL
               MOVE 0011               TO GFCTCJ-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB0A2'         TO WRK-NOME-TAB
               PERFORM 23210-MOVER-ERRO-SQL
           END-IF.

      *----------------------------------------------------------------*
       29300-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       30000-FINALIZAR                 SECTION.
      *----------------------------------------------------------------*

           GOBACK.

      *----------------------------------------------------------------*
       30000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

