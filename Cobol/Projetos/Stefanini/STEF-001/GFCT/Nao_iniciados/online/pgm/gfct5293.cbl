      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT5293.

      *================================================================*
      *                  SONDA PROCWORK INFORMATICA                    *
      *----------------------------------------------------------------*
      *                                                                *
      *    PROGRAMA.........: GFCT5293                                 *
      *    PROGRAMADOR......: FABRICA SOFTWARE        - SONDA PROCWORK *
      *                                                                *
      *    ANALISTA.........: MARCUS VINICIUS                          *
      *    LIDER PROJETO....: RODRIGO / MARCELO TEODORO                *
      *    GERENCIA / EQUIPE: GERENCIA-E / EQUIPE-1   - SONDA PROCWORK *
      *                                                                *
      *    GRUPO NEGOCIO....: DDS-GP50                                 *
      *    ANALISTA.........: HELENA                                   *
      *                                                                *
      *    DATA.............: MAR/2010                                 *
      *                                                                *
      *    PROJETO..........: ADESAO DE CESTAS CELULAR                 *
      *                                                                *
      *    OBJETIVO.........: GERAR DADOS PARA COMBO DE TARIFAS NA     *
      *                       FUNCIONALIDADE PERMISSAO DE PACOTES A    *
      *                       SEREM COBRADOS.                          *
      *                                                                *
      *    BANCO DE DADOS...:                                          *
      *      DB2                                                       *
      *      TABLE                             INCLUDE/BOOK            *
      *        DB2PRD.SERVC_TARIF_PRINC          GFCTB0D8              *
      *        DB2PRD.PARM_SERVC_TARIF           GFCTB0A2              *
      *        DB2PRD.PARM_DATA_PROCM            GFCTB0A1              *
      *        DB2PRD.TPRMSS_COBR_PCOTE          GFCTB0M5              *
      *        DB2PRD.TCREDT_OPER_FONE           GFCTB0M6              *
      *                                                                *
      *    BOOK'S...........:                                          *
      *        GFCTWAMC - ENT - AREA DE COMUNICACAO                    *
      *        GFCTWAMD - SAI - AREA DE COMUNICACAO                    *
      *        I#GFCTFZ - ENT - VERIFICAR DISPONIBILIDADE DO ON-LINE.  *
      *        I#GFCTG1 - SAI - VERIFICAR DISPONIBILIDADE DO ON-LINE.  *
      *        I#GFCTG2 - ENT - OBTER DESCRICAO DE MENSAGEM.           *
      *        I#GFCTG3 - SAI - OBTER DESCRICAO DE MENSAGEM.           *
      *        I#GFCT0M - AREA DE COMUNICACAO - ERROS.                 *
      *                                                                *
      *    MODULOS CHAMADOS.:                                          *
      *        GFCT5522 - VERIFICAR DISPONIBILIDADE DO ON-LINE.        *
      *        GFCT5523 - OBTER DESCRICAO DE MENSAGENS.                *
      *                                                                *
      *    NAVEGACAO                                                   *
      *        CHAMADO POR: GFCT7327                                   *
      *                                                                *
BI0115*----------------------------------------------------------------*
BI0115*                 U L T I M A   A L T E R A C A O                *
BI0115*----------------------------------------------------------------*
BI0115*                 SONDA IT  - ALTERACAO - BI0115                 *
BI0115*----------------------------------------------------------------*
BI0115*                                                                *
BI0115*    ANALISTA....:  UBIRAJARA(BIRA) - SONDA - IT                 *
BI0115*    DATA........:  01 / 2015                                    *
BI0115*    OBJETIVO....:  TRATAR TIPO DE TARIFA = 4                    *
BI0115*                   ( PACOTE PERSONALIZADO )                     *
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
           '*  INICIO DA WORKING GFCT5293  *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*         INDEXADORES          *'.
      *----------------------------------------------------------------*

       01  IND-1                       PIC 9(03) COMP-3    VALUE ZEROS.

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
       01  WRK-TAB                     PIC X(08)           VALUE SPACES.
       01  WRK-QTDE-TOT-REG            PIC 9(006) COMP-3   VALUE ZEROS.

BI0115 01  WRK-CTPO-SERVC-TARIF-2      PIC S9(001) COMP-3  VALUE +2.
BI0115 01  WRK-CTPO-SERVC-TARIF-4      PIC S9(001) COMP-3  VALUE +4.

       01  WRK-NRO-SEQ-X.
           03  WRK-NRO-SEQ             PIC 9(04)           VALUE ZEROS.

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

       01  WRK-S9-3                    PIC +9(009)         VALUE ZEROS.
       01  FILLER                      REDEFINES           WRK-S9-3.
           05  FILLER                  PIC  X(007).
           05  WRK-9-3                 PIC  9(003).

       01  WRK-AUX-5-S                 PIC +9(005)         VALUE ZEROS.
       01  FILLER                      REDEFINES           WRK-AUX-5-S.
           05  FILLER                  PIC  X(001).
           05  WRK-AUX-5               PIC  9(005).

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*     AREA DE MENSAGENS        *'.
      *----------------------------------------------------------------*

       01  WRK-MENSAGENS.
           03  WRK-MSG01.
               05  FILLER              PIC X(26)           VALUE
                   'ERRO NA CHAMADA DO MODULO'.
               05  WRK-MODULO-MSG01    PIC X(08)           VALUE SPACES.
               05  FILLER              PIC X(41)           VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*  AREA PARA MODULO GFCT5523   *'.
      *----------------------------------------------------------------*

       COPY 'I#GFCTG2'.

       COPY 'I#GFCTG3'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*  AREA PARA MODULO GFCT5522   *'.
      *----------------------------------------------------------------*

       COPY 'I#GFCTFZ'.

       COPY 'I#GFCTG1'.

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
                INCLUDE GFCTB0M5
           END-EXEC.

           EXEC SQL
               DECLARE CSR01-GFCTB0D8  CURSOR FOR
               SELECT
                       CSERVC_TARIF,
                       RSERVC_TARIF_REDZD,
                       CDEPDC
               FROM    DB2PRD.SERVC_TARIF_PRINC
               WHERE   CSERVC_TARIF      >= :GFCTB0D8.CSERVC-TARIF
BI0115         AND    (CTPO_SERVC_TARIF   = :WRK-CTPO-SERVC-TARIF-2
BI0115              OR CTPO_SERVC_TARIF   = :WRK-CTPO-SERVC-TARIF-4)
               ORDER   BY CSERVC_TARIF
           END-EXEC.

           EXEC SQL
               DECLARE CSR02-GFCTB0A2  CURSOR FOR
               SELECT
                       CSERVC_TARIF
               FROM    DB2PRD.PARM_SERVC_TARIF
               WHERE  (CSERVC_TARIF      >= :GFCTB0A2.CSERVC-TARIF)
                AND   (DINIC_VGCIA_TARIF <= :WRK-DATA-REFER
                AND    DFIM_VGCIA_TARIF  >= :WRK-DATA-REFER)
               ORDER   BY CSERVC_TARIF
           END-EXEC.

           EXEC SQL
               DECLARE CSR03-GFCTB0A2  CURSOR FOR
               SELECT
                       CSERVC_TARIF
               FROM    DB2PRD.PARM_SERVC_TARIF
               WHERE  (CSERVC_TARIF      >= :GFCTB0A2.CSERVC-TARIF)
                AND   (DFIM_VGCIA_TARIF  <  :WRK-DATA-REFER
                OR     DINIC_VGCIA_TARIF >  :WRK-DATA-REFER)
               ORDER   BY CSERVC_TARIF
           END-EXEC.

           EXEC SQL
               DECLARE CSR04-GFCTB0A2  CURSOR FOR
               SELECT
                       CSERVC_TARIF,
                       DINIC_VGCIA_TARIF
               FROM    DB2PRD.PARM_SERVC_TARIF
               WHERE ((CSERVC_TARIF      >= :GFCTB0A2.CSERVC-TARIF
                AND    DINIC_VGCIA_TARIF >= :WRK-DATA-PONTEIRO)
                 OR    CSERVC_TARIF      >  :GFCTB0A2.CSERVC-TARIF)
                AND  ((DINIC_VGCIA_TARIF <= :WRK-DATA-REFER
                AND    DFIM_VGCIA_TARIF  >= :WRK-DATA-REFER)
                 OR   (DINIC_VGCIA_TARIF >  :WRK-DATA-REFER))
               ORDER   BY CSERVC_TARIF,
                          DINIC_VGCIA_TARIF
           END-EXEC.

           EXEC SQL
               DECLARE CSR05-GFCTB0A2  CURSOR FOR
               SELECT
                       CSERVC_TARIF
               FROM    DB2PRD.PARM_SERVC_TARIF
               WHERE  (CSERVC_TARIF      >= :GFCTB0A2.CSERVC-TARIF)
                AND   (DFIM_VGCIA_TARIF  <  :WRK-DATA-REFER)
               ORDER   BY CSERVC_TARIF
           END-EXEC.

           EXEC SQL
               DECLARE CSR06-GFCTB0A2  CURSOR FOR
               SELECT
                       CSERVC_TARIF
               FROM    DB2PRD.PARM_SERVC_TARIF
               WHERE  (CSERVC_TARIF      >= :GFCTB0A2.CSERVC-TARIF)
                AND   (DINIC_VGCIA_TARIF >  :WRK-DATA-REFER)
               ORDER   BY CSERVC_TARIF
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)        VALUE
           '*    FIM DA WORKING GFCT5293   *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       COPY 'GFCTWAMC'.
       COPY 'GFCTWAMD'.
       COPY 'I#GFCT0M'.

      *================================================================*
       PROCEDURE DIVISION              USING WAMC-ENTRADA
                                             WAMD-SAIDA
                                             GFCT0M-AREA-ERROS.
      *================================================================*

      *----------------------------------------------------------------*
       0000-ROTINA-PRINCIPAL           SECTION.
      *----------------------------------------------------------------*

           PERFORM 1000-INICIALIZAR.

           PERFORM 2000-PROCESSAR.

           PERFORM 3000-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           PERFORM 1100-INICIALIZA-SAIDA.

           PERFORM 1200-CONSISTIR-DADOS.

           PERFORM 1300-VERIFICAR-ONLINE.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1100-INICIALIZA-SAIDA           SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WAMD-SAIDA.

           MOVE +268                   TO WAMD-LL.
           MOVE ZEROS                  TO WAMD-ZZ.
           MOVE WAMC-TRANSACAO         TO WAMD-TRANSACAO.
           MOVE WAMC-FUNCAO            TO WAMD-FUNCAO.

           IF  WAMC-QTDE-OCOR        NOT NUMERIC
               MOVE ZEROS              TO WAMD-QTDE-OCOR
           ELSE
               MOVE WAMC-QTDE-OCOR     TO WAMD-QTDE-OCOR
           END-IF.

           MOVE ZEROS                  TO WAMD-QTDE-TOT-REG
                                          WRK-QTDE-TOT-REG

           MOVE WAMC-FUNC-BDSCO        TO WAMD-FUNC-BDSCO.
           MOVE WAMC-TERMINAL          TO WAMD-TERMINAL.
           MOVE WAMC-PONTEIRO-TARIFA
                                       TO WAMD-PONTEIRO-TARIFA.
           MOVE WAMC-PONTEIRO-DATA     TO WAMD-PONTEIRO-DATA.

           MOVE 'N'                    TO WAMD-FIM.

           MOVE ZEROS                  TO WAMD-ERRO
                                          WAMD-COD-SQL-ERRO
                                          WAMD-COD-MSG-ERRO
                                          GFCTG2-COD-MSG.

           PERFORM 1110-OBTER-DESC-MSG.

           MOVE GFCTG3-DESC-MSG        TO WAMD-DESC-MSG-ERRO.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1110-OBTER-DESC-MSG             SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTG3-SAIDA.

           INITIALIZE GFCTG3-SAIDA.

           MOVE WAMC-TRANSACAO         TO GFCTG2-TRANSACAO.
           MOVE WAMC-FUNCAO            TO GFCTG2-FUNCAO.
           MOVE WAMC-FUNC-BDSCO        TO GFCTG2-FUNC-BDSCO.
           MOVE 'N'                    TO GFCTG2-FIM.
           MOVE 'O'                    TO GFCTG2-TIPO-PROC.

           MOVE 'GFCT5523'             TO WRK-MODULO.

           CALL WRK-MODULO                 USING GFCTG2-ENTRADA
                                                 GFCTG3-SAIDA
                                                 GFCT0M-AREA-ERROS.

           IF  RETURN-CODE                 NOT EQUAL ZEROS
               MOVE 9                  TO WAMD-ERRO
               MOVE WRK-MODULO         TO WRK-MODULO-MSG01
               MOVE WRK-MSG01          TO GFCT0M-TEXTO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT5293'         TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           ELSE
               IF  GFCTG3-ERRO             EQUAL 9
                   MOVE 9              TO WAMD-ERRO
                   IF  GFCT0M-TIPO-ACESSO  NOT EQUAL 'DB2'
                       MOVE WRK-MODULO TO GFCT0M-PROGRAMA
                       MOVE 'APL'      TO GFCT0M-TIPO-ACESSO
                       MOVE 'GFCT5293' TO GFCT0M-TRANSACAO
                   END-IF

                   PERFORM 3000-FINALIZAR
               END-IF
           END-IF.

           IF  GFCTG3-ERRO                 EQUAL 1
               MOVE 1                  TO WAMD-ERRO
               MOVE GFCTG3-COD-SQL-ERRO TO WAMD-COD-SQL-ERRO
               MOVE GFCTG3-COD-MSG-ERRO TO WAMD-COD-MSG-ERRO
               MOVE GFCTG3-DESC-MSG-ERRO TO WAMD-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1200-CONSISTIR-DADOS            SECTION.
      *----------------------------------------------------------------*

           IF (WAMC-TRANSACAO        EQUAL SPACES)          OR
              (WAMC-FUNCAO           EQUAL SPACES)          OR
              (WAMC-CSERVC-TARIF     IS NOT NUMERIC)        OR
              (WAMC-QTDE-OCOR        IS NOT NUMERIC)        OR
              (WAMC-QTDE-TOT-REG     IS NOT NUMERIC)        OR
              (WAMC-FUNC-BDSCO       EQUAL SPACES)          OR
              (WAMC-PONTEIRO-TARIFA  IS NOT NUMERIC)        OR
              (WAMC-TIPO-DATA        EQUAL  0               AND
               WAMC-PONTEIRO-DATA    EQUAL SPACES)          OR
              (WAMC-FIM              NOT EQUAL 'S' AND 'N') OR
              (WAMC-TIPO-VIGENCIA    NOT EQUAL 'V' AND 'N'  AND
                                                 'T' AND 'P'  AND
                                                 'H' AND 'F') OR
              (WAMC-TIPO-DATA        IS NOT NUMERIC         OR
               WAMC-TIPO-DATA        NOT EQUAL  0  AND  1   AND
                                                  2  AND  3 )
               MOVE 1                  TO WAMD-ERRO
               MOVE ZEROS              TO WAMD-COD-SQL-ERRO
               MOVE 0001               TO WAMD-COD-MSG-ERRO
               MOVE '0001'             TO GFCTG2-COD-MSG
               MOVE 'S'                TO WAMD-FIM
               PERFORM 1110-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO WAMD-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1300-VERIFICAR-ONLINE           SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTG1-SAIDA.

           INITIALIZE GFCTG1-SAIDA.

           MOVE WAMC-TRANSACAO         TO GFCTFZ-TRANSACAO.
           MOVE WAMC-FUNCAO            TO GFCTFZ-FUNCAO.
           MOVE WAMC-FUNC-BDSCO        TO GFCTFZ-FUNC-BDSCO.
           MOVE WAMC-FIM               TO GFCTFZ-FIM.
           MOVE 'GFCT'                 TO GFCTFZ-SISTEMA.

           MOVE 'GFCT5522'             TO WRK-MODULO.

           CALL WRK-MODULO                 USING GFCTFZ-ENTRADA
                                                 GFCTG1-SAIDA
                                                 GFCT0M-AREA-ERROS.

           IF  RETURN-CODE                 NOT EQUAL ZEROS
               MOVE 9                  TO WAMD-ERRO
               MOVE WRK-MODULO         TO WRK-MODULO-MSG01
               MOVE WRK-MSG01          TO GFCT0M-TEXTO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT5293'         TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           ELSE
               IF  GFCTG1-ERRO             EQUAL 9
                   MOVE 9              TO WAMD-ERRO
                   IF  GFCT0M-TIPO-ACESSO  NOT EQUAL 'DB2'
                       MOVE WRK-MODULO TO GFCT0M-PROGRAMA
                       MOVE 'APL'      TO GFCT0M-TIPO-ACESSO
                       MOVE 'GFCT5293' TO GFCT0M-TRANSACAO
                   END-IF

                   PERFORM 3000-FINALIZAR
               END-IF
           END-IF.

           IF  GFCTG1-CSIT-ONLINE          EQUAL 1
               MOVE 9                  TO WAMD-ERRO
               MOVE 'SISTEMA INDISPONIVEL' TO GFCT0M-TEXTO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT5293'         TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 2100-OBTER-COD-JUNCAO.

           PERFORM 2200-VERIFICAR-MASTER.

           PERFORM 2300-FORMATAR-DT-REF

           IF  WAMC-PONTEIRO-TARIFA  EQUAL ZEROS
               MOVE WAMC-CSERVC-TARIF
                                       TO WRK-AUX-TARIFA
               MOVE '01.01.0001'       TO WRK-DATA-PONTEIRO
           ELSE
               MOVE WAMC-PONTEIRO-TARIFA
                                       TO WRK-AUX-TARIFA
               MOVE WAMC-PONTEIRO-DATA
                                       TO WRK-DATA-PONTEIRO
           END-IF.

           EVALUATE WAMC-TIPO-VIGENCIA
               WHEN 'T'
                   MOVE 0235           TO WRK-NRO-SEQ
                   MOVE 'GFCTB0D8'     TO WRK-TAB
                   PERFORM 2400-TRATAR-CSR01
               WHEN 'V'
                   MOVE 0236           TO WRK-NRO-SEQ
                   MOVE 'GFCTB0A2'     TO WRK-TAB
                   PERFORM 2500-TRATAR-CSR02
               WHEN 'N'
                   MOVE 0237           TO WRK-NRO-SEQ
                   MOVE 'GFCTB0A2'     TO WRK-TAB
                   PERFORM 2600-TRATAR-CSR03
               WHEN 'P'
                   MOVE ZEROS          TO WRK-NRO-SEQ
                   MOVE 'GFCTB0A2'     TO WRK-TAB
                   PERFORM 2700-TRATAR-CSR04
               WHEN 'H'
                   MOVE 0239           TO WRK-NRO-SEQ
                   MOVE 'GFCTB0A2'     TO WRK-TAB
                   PERFORM 2800-TRATAR-CSR05
               WHEN OTHER
                   MOVE 0240           TO WRK-NRO-SEQ
                   MOVE 'GFCTB0A2'     TO WRK-TAB
                   PERFORM 2900-TRATAR-CSR06
           END-EVALUATE.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2100-OBTER-COD-JUNCAO           SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO WRK-VERSAO-0315
                                          WRK-COD-FUNCIONAL-0315
                                          WRK-NOME-FUNC-0315
                                          WRK-NOME-BANCO-0315
                                          WRK-NOME-DEPTO-0315
                                          WRK-STATUS-0315
                                          WRK-HIFEN-MENSAGEM-0315
                                          WRK-TEXTO-MENSAGEM-0315.
           MOVE ZEROS                  TO WRK-COD-JUNCAO-0315
                                          WRK-COD-BANCO-0315
                                          WRK-SECAO-0315
                                          WRK-COD-MENSAGEM-0315
                                          WRK-CNPJ-PRI-0315
                                          WRK-CNPJ-FIL-0315
                                          WRK-CNPJ-CON-0315.
           MOVE 'VRS001'               TO WRK-VERSAO-0315.
           MOVE WAMC-FUNC-BDSCO        TO WRK-COD-FUNCIONAL-0315.

           CALL 'SENH0315'          USING WRK-VERSAO-0315
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

           IF  RETURN-CODE            NOT EQUAL ZEROS AND 04
               MOVE 'S'                TO WAMD-FIM
               MOVE 1                  TO WAMD-ERRO
               MOVE RETURN-CODE        TO WAMD-COD-SQL-ERRO
               MOVE 0056               TO WAMD-COD-MSG-ERRO
               MOVE '0056'             TO GFCTG2-COD-MSG
               PERFORM 1110-OBTER-DESC-MSG
               MOVE GFCTG3-DESC-MSG    TO WAMD-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  RETURN-CODE                 EQUAL 04
               MOVE 'S'                TO WAMD-FIM
               IF  WRK-COD-MENSAGEM-0315   EQUAL 001
                   MOVE 1              TO WAMD-ERRO
                   MOVE ZEROS          TO WAMD-COD-SQL-ERRO
                   MOVE 0057           TO WAMD-COD-MSG-ERRO
                   MOVE '0057'         TO GFCTG2-COD-MSG
                   PERFORM 1110-OBTER-DESC-MSG
                   MOVE GFCTG3-DESC-MSG TO WAMD-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR
               END-IF

               IF  WRK-COD-MENSAGEM-0315   EQUAL 154
                   MOVE 1              TO WAMD-ERRO
                   MOVE ZEROS          TO WAMD-COD-SQL-ERRO
                   MOVE 0058           TO WAMD-COD-MSG-ERRO
                   MOVE '0058'         TO GFCTG2-COD-MSG
                   PERFORM 1110-OBTER-DESC-MSG
                   MOVE GFCTG3-DESC-MSG  TO WAMD-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR
               END-IF

               IF  WRK-COD-MENSAGEM-0315   EQUAL 242
                   MOVE 1              TO WAMD-ERRO
                   MOVE ZEROS          TO WAMD-COD-SQL-ERRO
                   MOVE 0059           TO WAMD-COD-MSG-ERRO
                   MOVE '0059'         TO GFCTG2-COD-MSG
                   PERFORM 1110-OBTER-DESC-MSG
                   MOVE GFCTG3-DESC-MSG TO WAMD-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR
               END-IF

               IF  WRK-COD-MENSAGEM-0315   EQUAL 009
                   MOVE ZEROS          TO WAMD-ERRO
                                          WAMD-COD-SQL-ERRO
                                          WAMD-COD-MSG-ERRO
                                          GFCTG2-COD-MSG
                   PERFORM 1110-OBTER-DESC-MSG
                   MOVE GFCTG3-DESC-MSG TO WAMD-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR
               END-IF

               IF  WRK-COD-MENSAGEM-0315   EQUAL 138
                   MOVE 1              TO WAMD-ERRO
                   MOVE ZEROS          TO WAMD-COD-SQL-ERRO
                   MOVE 0060           TO WAMD-COD-MSG-ERRO
                   MOVE '0060'         TO GFCTG2-COD-MSG
                   PERFORM 1110-OBTER-DESC-MSG
                   MOVE GFCTG3-DESC-MSG TO WAMD-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2200-VERIFICAR-MASTER           SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                  WRK-5507-AREA.

           MOVE 'O'                    TO WRK-5507-AMBIENTE.
           MOVE WRK-COD-JUNCAO-0315    TO WRK-5507-CJUNC-DEPDC.
           MOVE 'GFCT5507'             TO WRK-MODULO.

           CALL WRK-MODULO         USING     WRK-5507-AREA.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 9                  TO WAMD-ERRO

               MOVE 'GFCT5293'         TO GFCT0M-PROGRAMA
               MOVE 'GFCT5507'         TO WRK-MODULO-MSG01
               MOVE WRK-MSG01          TO GFCT0M-TEXTO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  WRK-5507-COD-RETORNO    NOT EQUAL ZEROS
               MOVE 'S'                TO WAMD-FIM
               IF  WRK-5507-COD-RETORNO
                                       EQUAL 01
                   MOVE 1              TO WAMD-ERRO
                   MOVE ZEROS          TO WAMD-COD-SQL-ERRO
                                          WAMD-COD-MSG-ERRO
                   MOVE WRK-5507-MSG-RETORNO
                                       TO WAMD-DESC-MSG-ERRO
               ELSE
                   IF  WRK-5507-COD-RETORNO
                                       EQUAL 99
                       MOVE 1          TO WAMD-ERRO
                       MOVE WRK-5507-COD-SQL-ERRO
                                       TO WAMD-COD-SQL-ERRO
                       MOVE ZEROS      TO WAMD-COD-MSG-ERRO
                       MOVE WRK-5507-MSG-RETORNO
                                       TO WAMD-DESC-MSG-ERRO
                   END-IF
               END-IF
               MOVE SPACES             TO WAMD-DADOS
               INITIALIZE WAMD-DADOS
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2300-FORMATAR-DT-REF            SECTION.
      *----------------------------------------------------------------*

           IF  WAMC-TIPO-DATA        EQUAL ZEROS
               MOVE WAMC-DATA-REFER    TO WRK-DATA-DD-MM-AAAA
               PERFORM 2310-FORMATAR-DT-WEB
               IF  WRK-ERRO-DATA       EQUAL 'S'
                   MOVE 1              TO WAMD-ERRO
                   MOVE ZEROS          TO WAMD-COD-SQL-ERRO
                   MOVE 0125           TO WAMD-COD-MSG-ERRO
                   MOVE '0125'         TO GFCTG2-COD-MSG
                   MOVE 'S'            TO WAMD-FIM
                   PERFORM 1110-OBTER-DESC-MSG
                   MOVE GFCTG3-DESC-MSG
                                       TO WAMD-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR
               ELSE
                   MOVE WRK-DATA-AAAA  TO WRK-DTREF-ANO
                   MOVE WRK-DATA-MM    TO WRK-DTREF-MES
                   MOVE WRK-DATA-DD    TO WRK-DTREF-DIA
                   MOVE '.'            TO WRK-P1
                                          WRK-P2
               END-IF
           ELSE
               PERFORM 2320-OBTER-DT-PROCE
               IF  WAMC-TIPO-DATA    EQUAL 1
                   MOVE DPROCM-ANTER OF GFCTB0A1
                                       TO WRK-DATA-REFER
               ELSE
                   IF  WAMC-TIPO-DATA
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
       2300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2310-FORMATAR-DT-WEB            SECTION.
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
       2310-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2320-OBTER-DT-PROCE             SECTION.
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
                                          WRK-S9-3
               MOVE WRK-9-3            TO WAMD-COD-SQL-ERRO
               MOVE '0010'             TO GFCT0M-LOCAL
               MOVE 0011               TO WAMD-COD-MSG-ERRO
               MOVE '0011'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0A1'         TO WRK-NOME-TAB
               PERFORM 2321-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2320-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2321-MOVER-ERRO-SQL             SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT5293'             TO GFCT0M-PROGRAMA.
           MOVE 'DB2'                  TO GFCT0M-TIPO-ACESSO.
           MOVE SQLCA                  TO GFCT0M-SQLCA-AREA.
           MOVE 9                      TO WAMD-ERRO.

           PERFORM 1110-OBTER-DESC-MSG.

           PERFORM 2321-1-CONCATENAR-MSG.

      *----------------------------------------------------------------*
       2321-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2321-1-CONCATENAR-MSG           SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO WAMD-DESC-MSG-ERRO.

           STRING GFCTG3-DESC-MSG ' - '
                  WRK-NOME-TAB
           DELIMITED BY '   '          INTO WAMD-DESC-MSG-ERRO.

      *----------------------------------------------------------------*
       2321-1-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2400-TRATAR-CSR01               SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                    TO WRK-FINALIZAR.

           PERFORM 2410-ABRIR-CSR01.

           PERFORM 2420-LER-CSR01.

           PERFORM VARYING IND-1       FROM 1 BY 1
                   UNTIL   IND-1       GREATER 7 OR
                         WRK-FINALIZAR EQUAL 'S'
               MOVE 'T'                TO WRK-TIPO-VIGENCIA
               PERFORM 2530-MOVER-CAMPOS-TARIFA
               PERFORM 2420-LER-CSR01
           END-PERFORM.

           IF  WRK-FINALIZAR           NOT EQUAL 'S'
               MOVE CSERVC-TARIF OF GFCTB0D8
                                       TO WRK-AUX-5-S
               MOVE WRK-AUX-5          TO WAMD-PONTEIRO-TARIFA
           END-IF.

           PERFORM 2440-FECHAR-CSR01.

      *----------------------------------------------------------------*
       2400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2410-ABRIR-CSR01                SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-AUX-TARIFA         TO CSERVC-TARIF     OF GFCTB0D8.

           EXEC SQL
               OPEN CSR01-GFCTB0D8
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'SERVC_TARIF_PRINC'
                                       TO GFCT0M-NOME-TAB
               MOVE 'OPEN'             TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          WRK-S9-3
               MOVE WRK-9-3            TO WAMD-COD-SQL-ERRO
               MOVE '0020'             TO GFCT0M-LOCAL
               MOVE 0011               TO WAMD-COD-MSG-ERRO
               MOVE '0011'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0B8'         TO WRK-NOME-TAB
               PERFORM 2321-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2410-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2420-LER-CSR01                  SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               FETCH CSR01-GFCTB0D8
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
                                          WRK-S9-3
               MOVE WRK-9-3            TO WAMD-COD-SQL-ERRO
               MOVE '0030'             TO GFCT0M-LOCAL
               MOVE 0006               TO WAMD-COD-MSG-ERRO
               MOVE '0006'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0D8'         TO WRK-NOME-TAB
               PERFORM 2321-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               PERFORM 2421-TRATAR-FIM-CSR
           ELSE
               MOVE CDEPDC OF GFCTB0D8 TO WRK-AUX-5-S
               IF (WRK-5507-IDTFD-MASTER
                                       EQUAL 'N' AND
                   WRK-COD-JUNCAO-0315 NOT EQUAL WRK-AUX-5)
                   GO                  TO 2420-LER-CSR01
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2420-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2421-TRATAR-FIM-CSR             SECTION.
      *----------------------------------------------------------------*

           MOVE 'S'                    TO WRK-FINALIZAR
                                          WAMD-FIM.

           IF  IND-1                   EQUAL ZEROS
               MOVE ZEROS              TO WAMD-ERRO
                                          WAMD-COD-SQL-ERRO
               MOVE WRK-NRO-SEQ        TO WAMD-COD-MSG-ERRO
               MOVE WRK-NRO-SEQ-X      TO GFCTG2-COD-MSG
               MOVE WRK-TAB            TO WRK-NOME-TAB
               PERFORM 1110-OBTER-DESC-MSG
               PERFORM 2321-1-CONCATENAR-MSG
           END-IF.

      *----------------------------------------------------------------*
       2421-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2440-FECHAR-CSR01               SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               CLOSE CSR01-GFCTB0D8
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'SERVC_TARIF_PRINC'
                                       TO GFCT0M-NOME-TAB
               MOVE 'CLOSE'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          WRK-S9-3
               MOVE WRK-9-3            TO WAMD-COD-SQL-ERRO
               MOVE '0040'             TO GFCT0M-LOCAL
               MOVE 0011               TO WAMD-COD-MSG-ERRO
               MOVE '0011'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0D8'         TO WRK-NOME-TAB
               PERFORM 2321-MOVER-ERRO-SQL
           END-IF.

      *----------------------------------------------------------------*
       2440-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2500-TRATAR-CSR02               SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                    TO WRK-FINALIZAR.

           PERFORM 2510-ABRIR-CSR02.

           PERFORM 2520-LER-CSR02.

           PERFORM                     VARYING IND-1 FROM 1 BY 1
                                       UNTIL IND-1 GREATER 7 OR
                                       WRK-FINALIZAR EQUAL 'S'
               MOVE 'V'                TO WRK-TIPO-VIGENCIA
               PERFORM 2530-MOVER-CAMPOS-TARIFA
               PERFORM 2520-LER-CSR02
           END-PERFORM.

           IF  WRK-FINALIZAR           NOT EQUAL 'S'
               MOVE CSERVC-TARIF       OF GFCTB0A2
                                       TO WRK-AUX-5-S
               MOVE WRK-AUX-5          TO WAMD-PONTEIRO-TARIFA
           END-IF.

           PERFORM 2540-FECHAR-CSR02.

      *----------------------------------------------------------------*
       2500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2510-ABRIR-CSR02                SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-AUX-TARIFA         TO CSERVC-TARIF      OF GFCTB0A2.

           EXEC SQL
               OPEN CSR02-GFCTB0A2
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'PARM_SERVC_TARIF' TO GFCT0M-NOME-TAB
               MOVE 'OPEN'             TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          WRK-S9-3
               MOVE WRK-9-3            TO WAMD-COD-SQL-ERRO
               MOVE '0050'             TO GFCT0M-LOCAL
               MOVE 0005               TO WAMD-COD-MSG-ERRO
               MOVE '0005'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0A2'         TO WRK-NOME-TAB
               PERFORM 2321-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2510-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2520-LER-CSR02                  SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               FETCH CSR02-GFCTB0A2
               INTO   :GFCTB0A2.CSERVC-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'PARM_SERVC_TARIF' TO GFCT0M-NOME-TAB
               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          WRK-S9-3
               MOVE WRK-9-3            TO WAMD-COD-SQL-ERRO
               MOVE '0060'             TO GFCT0M-LOCAL
               MOVE 0006               TO WAMD-COD-MSG-ERRO
               MOVE '0006'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0A2'         TO WRK-NOME-TAB
               PERFORM 2321-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               PERFORM 2421-TRATAR-FIM-CSR
           ELSE
               PERFORM 2531-OBTER-DESC-TARIFA

BI0115         IF  CTPO-SERVC-TARIF OF GFCTB0D8  =  +2  OR  +4
                   MOVE CDEPDC OF GFCTB0D8 TO WRK-AUX-5-S

                   IF (WRK-5507-IDTFD-MASTER
                                       EQUAL 'N' AND
                       WRK-COD-JUNCAO-0315 NOT EQUAL WRK-AUX-5)
                       GO              TO 2520-LER-CSR02
                   END-IF
               ELSE
                   GO                  TO 2520-LER-CSR02
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2520-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2530-MOVER-CAMPOS-TARIFA        SECTION.
      *----------------------------------------------------------------*

           PERFORM 2532-ACESSAR-GFCTB0M5

           IF  SQLCODE            EQUAL +100

               ADD 106                 TO WAMD-LL

               MOVE CSERVC-TARIF       OF GFCTB0D8
                                       TO WRK-AUX-5-S
               MOVE WRK-AUX-5          TO WAMD-COD-TARIFA(IND-1)
               MOVE RSERVC-TARIF-REDZD OF GFCTB0D8
                                       TO WAMD-DESC-TARIFA(IND-1)
               MOVE WRK-TIPO-VIGENCIA  TO WAMD-TIPO-VIGENCIA(IND-1)

               ADD 1                   TO WRK-QTDE-TOT-REG
               MOVE WRK-QTDE-TOT-REG   TO WAMD-QTDE-TOT-REG
           ELSE
               COMPUTE   IND-1  =  IND-1 - 1
           END-IF.

      *----------------------------------------------------------------*
       2530-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2531-OBTER-DESC-TARIFA          SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF OF GFCTB0A2
                                       TO CSERVC-TARIF OF GFCTB0D8.

           EXEC SQL
               SELECT  CDEPDC,
                       CSERVC_TARIF,
                       RSERVC_TARIF_REDZD,
                       CTPO_SERVC_TARIF
               INTO    :GFCTB0D8.CDEPDC,
                       :GFCTB0D8.CSERVC-TARIF,
                       :GFCTB0D8.RSERVC-TARIF-REDZD,
                       :GFCTB0D8.CTPO-SERVC-TARIF
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
                                          WRK-S9-3
               MOVE WRK-9-3            TO WAMD-COD-SQL-ERRO
               MOVE '0090'             TO GFCT0M-LOCAL
               MOVE 0009               TO WAMD-COD-MSG-ERRO
               MOVE '0009'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0D8'         TO WRK-NOME-TAB
               PERFORM 2321-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FINALIZAR
                                          WAMD-FIM
               IF  IND-1               EQUAL ZEROS
                   MOVE 1              TO WAMD-ERRO
                   MOVE ZEROS          TO WAMD-COD-SQL-ERRO
                   MOVE 0242           TO WAMD-COD-MSG-ERRO
                   MOVE '0242'         TO GFCTG2-COD-MSG
                   PERFORM 1110-OBTER-DESC-MSG
                   MOVE GFCTG3-DESC-MSG  TO WAMD-DESC-MSG-ERRO
                   PERFORM 3000-FINALIZAR
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2531-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2532-ACESSAR-GFCTB0M5           SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF           OF GFCTB0D8
                                       TO CSERVC-TARIF     OF GFCTB0M5
           MOVE DPROCM-ATUAL           OF GFCTB0A1
                                       TO DFIM-VGCIA-REG   OF GFCTB0M5

           EXEC SQL
               SELECT
                       CSERVC_TARIF
               INTO
                       :GFCTB0M5.CSERVC-TARIF
               FROM    DB2PRD.TPRMSS_COBR_PCOTE
              WHERE    CSERVC_TARIF      = :GFCTB0M5.CSERVC-TARIF
                AND    DFIM_VGCIA_REG    > :GFCTB0M5.DFIM-VGCIA-REG
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100
                                                       AND -811 ) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'TPRMSS_COBR_PCOTE '
                                       TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          WRK-S9-3
               MOVE WRK-9-3            TO WAMD-COD-SQL-ERRO
               MOVE '0080'             TO GFCT0M-LOCAL
               MOVE 0009               TO WAMD-COD-MSG-ERRO
               MOVE '0009'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0M5'         TO WRK-NOME-TAB
               PERFORM 2321-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2532-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2540-FECHAR-CSR02               SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               CLOSE CSR02-GFCTB0A2
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'PARM_SERVC_TARIF' TO GFCT0M-NOME-TAB
               MOVE 'CLOSE'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          WRK-S9-3
               MOVE WRK-9-3            TO WAMD-COD-SQL-ERRO
               MOVE '0100'             TO GFCT0M-LOCAL
               MOVE 0011               TO WAMD-COD-MSG-ERRO
               MOVE '0011'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0A2'         TO WRK-NOME-TAB
               PERFORM 2321-MOVER-ERRO-SQL
           END-IF.

      *----------------------------------------------------------------*
       2540-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2600-TRATAR-CSR03               SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                    TO WRK-FINALIZAR.

           PERFORM 2610-ABRIR-CSR03.

           PERFORM 2620-LER-CSR03.

           PERFORM                     VARYING IND-1 FROM 1 BY 1
                                       UNTIL IND-1 GREATER 7 OR
                                       WRK-FINALIZAR EQUAL 'S'
               MOVE ' '                TO WRK-TIPO-VIGENCIA
               PERFORM 2530-MOVER-CAMPOS-TARIFA
               PERFORM 2620-LER-CSR03
           END-PERFORM.

           IF  WRK-FINALIZAR           NOT EQUAL 'S'
               MOVE CSERVC-TARIF OF GFCTB0A2
                                       TO WRK-AUX-5-S
               MOVE WRK-AUX-5          TO WAMD-PONTEIRO-TARIFA
           END-IF.

           PERFORM 2630-FECHAR-CSR03.

      *----------------------------------------------------------------*
       2600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2610-ABRIR-CSR03                SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-AUX-TARIFA         TO CSERVC-TARIF      OF GFCTB0A2.

           EXEC SQL
               OPEN CSR03-GFCTB0A2
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'PARM_SERVC_TARIF' TO GFCT0M-NOME-TAB
               MOVE 'OPEN'             TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          WRK-S9-3
               MOVE WRK-9-3            TO WAMD-COD-SQL-ERRO
               MOVE '0110'             TO GFCT0M-LOCAL
               MOVE 0005               TO WAMD-COD-MSG-ERRO
               MOVE '0005'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0A2'         TO WRK-NOME-TAB
               PERFORM 2321-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2610-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2620-LER-CSR03                  SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               FETCH CSR03-GFCTB0A2
               INTO  :GFCTB0A2.CSERVC-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'PARM_SERVC_TARIF' TO GFCT0M-NOME-TAB
               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          WRK-S9-3
               MOVE WRK-9-3            TO WAMD-COD-SQL-ERRO
               MOVE '0120'             TO GFCT0M-LOCAL
               MOVE 0006               TO WAMD-COD-MSG-ERRO
               MOVE '0006'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0A2'         TO WRK-NOME-TAB
               PERFORM 2321-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               PERFORM 2421-TRATAR-FIM-CSR
           ELSE
               PERFORM 2531-OBTER-DESC-TARIFA

BI0115         IF  CTPO-SERVC-TARIF OF GFCTB0D8  =  +2  OR  +4
                   MOVE CDEPDC OF GFCTB0D8 TO WRK-AUX-5-S

                   IF (WRK-5507-IDTFD-MASTER
                                       EQUAL 'N' AND
                       WRK-COD-JUNCAO-0315 NOT EQUAL WRK-AUX-5)
                       GO              TO 2620-LER-CSR03
                   END-IF
               ELSE
                   GO                  TO 2620-LER-CSR03
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2620-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2630-FECHAR-CSR03               SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               CLOSE CSR03-GFCTB0A2
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'PARM_SERVC_TARIF' TO GFCT0M-NOME-TAB
               MOVE 'CLOSE'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          WRK-S9-3
               MOVE WRK-9-3            TO WAMD-COD-SQL-ERRO
               MOVE '0130'             TO GFCT0M-LOCAL
               MOVE 0011               TO WAMD-COD-MSG-ERRO
               MOVE '0011'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0A2'         TO WRK-NOME-TAB
               PERFORM 2321-MOVER-ERRO-SQL
           END-IF.

      *----------------------------------------------------------------*
       2630-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2700-TRATAR-CSR04               SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                    TO WRK-FINALIZAR.

           PERFORM 2710-ABRIR-CSR04.

           PERFORM 2720-LER-CSR04.

           PERFORM                     VARYING IND-1 FROM 1 BY 1
                                       UNTIL IND-1 GREATER 7 OR
                                       WRK-FINALIZAR EQUAL 'S'
               PERFORM 2730-INVERTER-DATAS
               IF  WRK-DT-DINIC        GREATER WRK-DT-REFER
                   MOVE 'P'            TO WRK-TIPO-VIGENCIA
               ELSE
                   MOVE 'V'            TO WRK-TIPO-VIGENCIA
               END-IF
               PERFORM 2530-MOVER-CAMPOS-TARIFA
               PERFORM 2720-LER-CSR04
           END-PERFORM.

           IF  WRK-FINALIZAR           NOT EQUAL 'S'
               MOVE CSERVC-TARIF       OF GFCTB0A2
                                       TO WRK-AUX-5-S
               MOVE WRK-AUX-5          TO WAMD-PONTEIRO-TARIFA
               MOVE DINIC-VGCIA-TARIF  OF GFCTB0A2
                                       TO WAMD-PONTEIRO-DATA
           END-IF.

           PERFORM 2740-FECHAR-CSR04.

      *----------------------------------------------------------------*
       2700-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2710-ABRIR-CSR04                SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-AUX-TARIFA         TO CSERVC-TARIF     OF GFCTB0A2.

           EXEC SQL
               OPEN CSR04-GFCTB0A2
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'PARM_SERVC_TARIF' TO GFCT0M-NOME-TAB
               MOVE 'OPEN'             TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          WRK-S9-3
               MOVE WRK-9-3            TO WAMD-COD-SQL-ERRO
               MOVE '0140'             TO GFCT0M-LOCAL
               MOVE 0005               TO WAMD-COD-MSG-ERRO
               MOVE '0005'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0A2'         TO WRK-NOME-TAB
               PERFORM 2321-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2710-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2720-LER-CSR04                  SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               FETCH CSR04-GFCTB0A2
               INTO :GFCTB0A2.CSERVC-TARIF,
                    :GFCTB0A2.DINIC-VGCIA-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'PARM_SERVC_TARIF' TO GFCT0M-NOME-TAB
               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          WRK-S9-3
               MOVE WRK-9-3            TO WAMD-COD-SQL-ERRO
               MOVE '0150'             TO GFCT0M-LOCAL
               MOVE 0006               TO WAMD-COD-MSG-ERRO
               MOVE '0006'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0A2'         TO WRK-NOME-TAB
               PERFORM 2321-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               PERFORM 2421-TRATAR-FIM-CSR
           ELSE
               PERFORM 2531-OBTER-DESC-TARIFA

BI0115         IF  CTPO-SERVC-TARIF OF GFCTB0D8  =  +2  OR  +4
                   MOVE CDEPDC OF GFCTB0D8 TO WRK-AUX-5-S

                   IF (WRK-5507-IDTFD-MASTER
                                       EQUAL 'N' AND
                       WRK-COD-JUNCAO-0315 NOT EQUAL WRK-AUX-5)
                       GO              TO 2720-LER-CSR04
                   END-IF
               ELSE
                   GO                  TO 2720-LER-CSR04
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2720-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2730-INVERTER-DATAS             SECTION.
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
       2730-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2740-FECHAR-CSR04               SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               CLOSE CSR04-GFCTB0A2
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'PARM_SERVC_TARIF' TO GFCT0M-NOME-TAB
               MOVE 'CLOSE'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          WRK-S9-3
               MOVE WRK-9-3            TO WAMD-COD-SQL-ERRO
               MOVE '0160'             TO GFCT0M-LOCAL
               MOVE 0011               TO WAMD-COD-MSG-ERRO
               MOVE '0011'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0A2'         TO WRK-NOME-TAB
               PERFORM 2321-MOVER-ERRO-SQL
           END-IF.

      *----------------------------------------------------------------*
       2740-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2800-TRATAR-CSR05               SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                    TO WRK-FINALIZAR.

           PERFORM 2810-ABRIR-CSR05.

           PERFORM 2820-LER-CSR05.

           PERFORM                     VARYING IND-1 FROM 1 BY 1
                                       UNTIL IND-1 GREATER 7 OR
                                       WRK-FINALIZAR EQUAL 'S'
               MOVE 'H'                TO WRK-TIPO-VIGENCIA
               PERFORM 2530-MOVER-CAMPOS-TARIFA
               PERFORM 2820-LER-CSR05
           END-PERFORM.

           IF  WRK-FINALIZAR           NOT EQUAL 'S'
               MOVE CSERVC-TARIF       OF GFCTB0A2
                                       TO WRK-AUX-5-S
               MOVE WRK-AUX-5          TO WAMD-PONTEIRO-TARIFA
           END-IF.

           PERFORM 2830-FECHAR-CSR05.

      *----------------------------------------------------------------*
       2800-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2810-ABRIR-CSR05                SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-AUX-TARIFA         TO CSERVC-TARIF     OF GFCTB0A2.

           EXEC SQL
               OPEN CSR05-GFCTB0A2
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'PARM_SERVC_TARIF' TO GFCT0M-NOME-TAB
               MOVE 'OPEN'             TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          WRK-S9-3
               MOVE WRK-9-3            TO WAMD-COD-SQL-ERRO
               MOVE '0170'             TO GFCT0M-LOCAL
               MOVE 0005               TO WAMD-COD-MSG-ERRO
               MOVE '0005'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0A2'         TO WRK-NOME-TAB
               PERFORM 2321-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2810-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2820-LER-CSR05                  SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               FETCH CSR05-GFCTB0A2
               INTO  :GFCTB0A2.CSERVC-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'PARM_SERVC_TARIF' TO GFCT0M-NOME-TAB
               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          WRK-S9-3
               MOVE WRK-9-3            TO WAMD-COD-SQL-ERRO
               MOVE '0180'             TO GFCT0M-LOCAL
               MOVE 0006               TO WAMD-COD-MSG-ERRO
               MOVE '0006'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0A2'         TO WRK-NOME-TAB
               PERFORM 2321-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               PERFORM 2421-TRATAR-FIM-CSR
           ELSE
               PERFORM 2531-OBTER-DESC-TARIFA

BI0115         IF  CTPO-SERVC-TARIF OF GFCTB0D8  =  +2  OR  +4
                   MOVE CDEPDC OF GFCTB0D8 TO WRK-AUX-5-S

                   IF (WRK-5507-IDTFD-MASTER
                                       EQUAL 'N' AND
                       WRK-COD-JUNCAO-0315 NOT EQUAL WRK-AUX-5)
                       GO              TO 2820-LER-CSR05
                   END-IF
               ELSE
                   GO                  TO 2820-LER-CSR05
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2820-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2830-FECHAR-CSR05               SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               CLOSE CSR05-GFCTB0A2
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'PARM_SERVC_TARIF' TO GFCT0M-NOME-TAB
               MOVE 'CLOSE'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          WRK-S9-3
               MOVE WRK-9-3            TO WAMD-COD-SQL-ERRO
               MOVE '0190'             TO GFCT0M-LOCAL
               MOVE 0011               TO WAMD-COD-MSG-ERRO
               MOVE '0011'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0A2'         TO WRK-NOME-TAB
               PERFORM 2321-MOVER-ERRO-SQL
           END-IF.

      *----------------------------------------------------------------*
       2830-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2900-TRATAR-CSR06               SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                    TO WRK-FINALIZAR.

           PERFORM 2910-ABRIR-CSR06.

           PERFORM 2920-LER-CSR06.

           PERFORM                     VARYING IND-1 FROM 1 BY 1
                                       UNTIL IND-1 GREATER 7 OR
                                       WRK-FINALIZAR EQUAL 'S'
               MOVE 'F'                TO WRK-TIPO-VIGENCIA
               PERFORM 2530-MOVER-CAMPOS-TARIFA
               PERFORM 2920-LER-CSR06
           END-PERFORM.

           IF  WRK-FINALIZAR           NOT EQUAL 'S'
               MOVE CSERVC-TARIF       OF GFCTB0A2
                                       TO WRK-AUX-5-S
               MOVE WRK-AUX-5          TO WAMD-PONTEIRO-TARIFA
           END-IF.

           PERFORM 2930-FECHAR-CSR06.

      *----------------------------------------------------------------*
       2900-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2910-ABRIR-CSR06                SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-AUX-TARIFA         TO CSERVC-TARIF     OF GFCTB0A2.

           EXEC SQL
               OPEN CSR06-GFCTB0A2
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'PARM_SERVC_TARIF' TO GFCT0M-NOME-TAB
               MOVE 'OPEN'             TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          WRK-S9-3
               MOVE WRK-9-3            TO WAMD-COD-SQL-ERRO
               MOVE '0200'             TO GFCT0M-LOCAL
               MOVE 0005               TO WAMD-COD-MSG-ERRO
               MOVE '0005'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0A2'         TO WRK-NOME-TAB
               PERFORM 2321-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2910-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2920-LER-CSR06                  SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               FETCH CSR06-GFCTB0A2
               INTO  :GFCTB0A2.CSERVC-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'PARM_SERVC_TARIF' TO GFCT0M-NOME-TAB
               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          WRK-S9-3
               MOVE WRK-9-3            TO WAMD-COD-SQL-ERRO
               MOVE '0210'             TO GFCT0M-LOCAL
               MOVE 0006               TO WAMD-COD-MSG-ERRO
               MOVE '0006'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0A2'         TO WRK-NOME-TAB
               PERFORM 2321-MOVER-ERRO-SQL
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               PERFORM 2421-TRATAR-FIM-CSR
           ELSE
               PERFORM 2531-OBTER-DESC-TARIFA

BI0115         IF  CTPO-SERVC-TARIF OF GFCTB0D8  =  +2  OR  +4
                   MOVE CDEPDC OF GFCTB0D8 TO WRK-AUX-5-S

                   IF (WRK-5507-IDTFD-MASTER
                                       EQUAL 'N' AND
                       WRK-COD-JUNCAO-0315 NOT EQUAL WRK-AUX-5)
                       GO              TO 2920-LER-CSR06
                   END-IF
               ELSE
                   GO                  TO 2920-LER-CSR06
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2920-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2930-FECHAR-CSR06               SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               CLOSE CSR06-GFCTB0A2
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE GFCT0M-ERRO-SQL
               MOVE 'PARM_SERVC_TARIF' TO GFCT0M-NOME-TAB
               MOVE 'CLOSE'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
                                          WRK-S9-3
               MOVE WRK-9-3            TO WAMD-COD-SQL-ERRO
               MOVE '0220'             TO GFCT0M-LOCAL
               MOVE 0011               TO WAMD-COD-MSG-ERRO
               MOVE '0011'             TO GFCTG2-COD-MSG
               MOVE 'GFCTB0A2'         TO WRK-NOME-TAB
               PERFORM 2321-MOVER-ERRO-SQL
           END-IF.

      *----------------------------------------------------------------*
       2930-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           GOBACK.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
