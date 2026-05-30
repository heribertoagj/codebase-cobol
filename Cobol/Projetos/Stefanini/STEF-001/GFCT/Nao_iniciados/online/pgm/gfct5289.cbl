      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT5289.

      *================================================================*
      *                  SONDA PROCWORK INFORMATICA                    *
      *----------------------------------------------------------------*
      *                                                                *
      *    PROGRAMA.........: GFCT5289                                 *
      *    PROGRAMADOR......: FABRICA SOFTWARE        - SONDA PROCWORK *
      *                                                                *
      *    ANALISTA.........: MARCUS VINICIUS                          *
      *    LIDER PROJETO....: RODRIGO / MARCELO TEODORO                *
      *    GERENCIA / EQUIPE: GERENCIA-E / EQUIPE-1   - SONDA PROCWORK *
      *                                                                *
      *    GRUPO NEGOCIO....: DDS-GP50                                 *
      *    ANALISTA.........: HELENA                                   *
      *                                                                *
      *    DATA.............: FEV/2010                                 *
      *                                                                *
      *    PROJETO..........: ADESAO DE CESTAS CELULAR                 *
      *                                                                *
      *    OBJETIVO.........: GERAR CONSULTA DA ALTERACAO DO NUMERO DO *
      *                       CELULAR E O PACOTE.                      *
      *                                                                *
      *    BANCO DE DADOS...:                                          *
      *      DB2                                                       *
      *      TABLE                             INCLUDE/BOOK            *
      *        DB2PRD.ADSAO_INDVD_PCOTE          GFCTB009              *
      *        DB2PRD.SERVC_TARIF_PRINC          GFCTB0D8              *
      *        DB2PRD.TADSAO_INDVD_CLULR         GFCTB0M2              *
      *        DB2PRD.PARM_DATA_PROCM            GFCTB0A1              *
      *        DB2PRD.TOPER_FONE_CREDT           GFCTB0M4              *
      *                                                                *
      *    BOOK'S...........:                                          *
      *        GFCTWAMS - ENT                                          *
      *        GFCTWAMT - SAI                                          *
      *        I#GFCTFZ - ENT - VERIFICAR DISPONIBILIDADE DO ON-LINE.  *
      *        I#GFCTG1 - SAI - VERIFICAR DISPONIBILIDADE DO ON-LINE.  *
      *        I#GFCTG2 - ENT - OBTER DESCRICAO DE MENSAGEM.           *
      *        I#GFCTG3 - SAI - OBTER DESCRICAO DE MENSAGEM.           *
      *        I#GFCT0M - AREA DE COMUNICACAO - ERROS.                 *
      *                                                                *
      *    MODULOS CHAMADOS.:                                          *
      *        GFCT5522 - VERIFICAR DISPONIBILIDADE DO ON-LINE.        *
      *        GFCT5523 - OBTER DESCRICAO DE MENSAGENS.                *
      *        GFCT5054 - OBTER DADOS DO CLIENTE.                      *
      *        POOL0025 - SUBTRAIR DIAS DE UMA DATA.                   *
      *                                                                *
      *================================================================*
022012*----------------------------------------------------------------*
022012*                 U L T I M A   A L T E R A C A O                *
022012*----------------------------------------------------------------*
022012*        SONDA PROCWORK - CONSULTORIA - ALTERACAO                *
022012*----------------------------------------------------------------*
022012*                                                                *
022012*    PROGRAMADOR.:  FABRICA         - SONDA/PROCWORK             *
022012*    ANALISTA....:  PAGNOCCA        - SONDA/PROCWORK             *
022012*    DATA........:  FEV / 2012                                   *
022012*    OBJETIVO....:  PROJETO CELULAR 09 DIGITOS.                  *
022012*                                                                *
022012*================================================================*

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
           'AREA PARA VARIAVEIS AUXILIARES'.
      *----------------------------------------------------------------*

       77  WRK-MODULO                  PIC  X(008)         VALUE SPACES.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           'AREA DE VARIAVEIS DE REDEFINICAO'.
      *----------------------------------------------------------------*

       01  WRK-AUX-3-S                 PIC -9(003)         VALUE ZEROS.
       01  FILLER                      REDEFINES           WRK-AUX-3-S.
           05  FILLER                  PIC  X(001).
           05  WRK-AUX-3               PIC  9(003).

       01  WRK-AUX-5-S                 PIC -9(005)         VALUE ZEROS.
       01  FILLER                      REDEFINES           WRK-AUX-5-S.
           05  FILLER                  PIC  X(001).
           05  WRK-AUX-5               PIC  9(005).

022012 01  WRK-AUX-11-S                PIC -9(011)         VALUE ZEROS.
022012 01  FILLER                      REDEFINES           WRK-AUX-11-S.
           05  FILLER                  PIC  X(001).
022012     05  WRK-AUX-11              PIC  9(011).

       01  WRK-AUX-9-S                 PIC -9(009)         VALUE ZEROS.
       01  FILLER                      REDEFINES           WRK-AUX-9-S.
           05  FILLER                  PIC  X(001).
           05  WRK-AUX-9               PIC  9(009).

       01  WRK-AUXILIAR-4              PIC +9(009)         VALUE ZEROS.
       01  FILLER                      REDEFINES        WRK-AUXILIAR-4.
           05  FILLER                  PIC  X(001).
           05  FILLER                  PIC  9(006).
           05  WRK-AUX-4               PIC  9(003).

       01  WRK-MSG-AUX.
           05 WRK-MSG-75               PIC  X(075)         VALUE SPACES.
           05 FILLER                   PIC  X(004)         VALUE SPACES.

       01  WRK-POOL0025.
           05  WRK-P0025-DATA-ENT      PIC  9(008)         VALUE ZEROS.
           05  WRK-P0025-DIAS          PIC S9(005) COMP-3  VALUE ZEROS.
           05  WRK-P0025-DATA-SAI      PIC  9(008)         VALUE ZEROS.

       01  WRK-DATA-AMD                PIC  9(008)         VALUE ZEROS.
       01  FILLER         REDEFINES    WRK-DATA-AMD.
           05  WRK-ANO-AMD             PIC  9(004).
           05  WRK-MES-AMD             PIC  9(002).
           05  WRK-DIA-AMD             PIC  9(002).

       01  WRK-MES-COMP                PIC  9(002) COMP-3  VALUE ZEROS.
       01  WRK-ANO-COMP                PIC  9(004) COMP-3  VALUE ZEROS.

       01  WRK-DATA-DB2.
           05  WRK-DIA-DB2             PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE '.'.
           05  WRK-MES-DB2             PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE '.'.
           05  WRK-ANO-DB2             PIC  9(004)         VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           'AREA DE VARIAVEIS DE MENSAGEM'.
      *----------------------------------------------------------------*

       01  WRK-MSG01.
           05  FILLER                  PIC  X(026)         VALUE
               'ERRO NA CHAMADA DO MODULO'.
           05  WRK-NOME-MOD            PIC  X(008)         VALUE SPACES.

       01  WRK-NOME-TAB                PIC  X(011)         VALUE SPACES.

       01  WRK-MSG02                   PIC  X(026)         VALUE
               'SISTEMA INDISPONIVEL'.

       01  WRK-MSG05.
           05  WRK-MSG05-MODULO        PIC  X(08)          VALUE SPACES.
           05  FILLER                  PIC  X(67)          VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           'AREA DE ACESSO AO MODULO GFCT5523'.
      *----------------------------------------------------------------*

       COPY 'I#GFCTG2'.

       COPY 'I#GFCTG3'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           'AREA DE ACESSO AO MODULO GFCT5522'.
      *----------------------------------------------------------------*

       COPY 'I#GFCTFZ'.

       COPY 'I#GFCTG1'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           'AREA DE ACESSO AO MODULO GFCT5054'.
      *----------------------------------------------------------------*

       COPY 'I#GFCTG0'.

       COPY 'I#GFCTHA'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(020)         VALUE
           'AREA PARA DB2'.
      *----------------------------------------------------------------*

           EXEC SQL
             INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
             INCLUDE CLIEV004
           END-EXEC.

           EXEC SQL
             INCLUDE CLIEV007
           END-EXEC.

           EXEC SQL
             INCLUDE CLIEV008
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB009
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0D8
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0M2
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0A1
           END-EXEC.

           EXEC SQL
             INCLUDE GFCTB0M4
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA CURSORES *'.
      *----------------------------------------------------------------*

           EXEC SQL
             DECLARE CSR01-GFCTB0M2 CURSOR  FOR
             SELECT
                     NSEQ_REG_CLULR,
                     COPER_FONE_MOVEL,
                     CDDD_FONE_MOVEL,
022012               NLIN_TFONI
             FROM
                     DB2PRD.TADSAO_INDVD_CLULR
             WHERE
                     CAG_BCRIA        = :GFCTB0M2.CAG-BCRIA
               AND   CCTA_CLI         = :GFCTB0M2.CCTA-CLI
               AND   CSERVC_TARIF     = :GFCTB0M2.CSERVC-TARIF
               AND   HINCL_REG        = :GFCTB0M2.HINCL-REG
             ORDER BY
                     NSEQ_REG_CLULR DESC
           END-EXEC.

           EXEC SQL
             DECLARE CSR02-GFCTB009 CURSOR  FOR
             SELECT
                     CSERVC_TARIF ,
                     HINCL_REG ,
                     DINIC_ADSAO_INDVD ,
                     DFIM_ADSAO_INDVD
             FROM
                     DB2PRD.ADSAO_INDVD_PCOTE
             WHERE
                     CJUNC_DEPDC       = :GFCTB009.CJUNC-DEPDC
               AND   CCTA_CLI          = :GFCTB009.CCTA-CLI
               AND   CINDCD_EXCL_REG   = :GFCTB009.CINDCD-EXCL-REG
               AND   DFIM_ADSAO_INDVD  > :GFCTB009.DFIM-ADSAO-INDVD
             ORDER BY
                     HINCL_REG DESC
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(030)         VALUE
           'FIM DA WORKING STORAGE SECTION'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       COPY 'GFCTWAMS'.

       COPY 'GFCTWAMT'.

       COPY 'I#GFCT0M'.

      *================================================================*
       PROCEDURE                       DIVISION USING GFCTWAMS-ENTRADA
                                                      GFCTWAMT-SAIDA
                                                      GFCT0M-AREA-ERROS.
      *================================================================*

      ******************************************************************
      *    FLUXO DE PROCESSAMENTO DO PROGRAMA.                         *
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
      *    ROTINA QUE CHAMA OS PROCEDIMENTOS INICIAIS DO PROGRAMA.     *
      ******************************************************************
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO  GFCT0M-AREA-ERROS
                                           GFCTWAMT-SAIDA.

           INITIALIZE                  GFCTWAMT-SAIDA
                                       GFCT0M-AREA-ERROS.

           PERFORM 1100-INICIALIZAR-AREA-SAIDA.

           PERFORM 1200-VERIFICAR-SIST-DISPONIVEL.

           PERFORM 1300-EFETUAR-CONSISTENCIA.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    INICILIZA AS AREAS DE SAIDA                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       1100-INICIALIZAR-AREA-SAIDA     SECTION.
      *----------------------------------------------------------------*

           MOVE 1010                   TO WAMT-LL.
           MOVE ZEROS                  TO WAMT-ZZ

           MOVE WAMS-TRANSACAO         TO WAMT-TRANSACAO.
           MOVE WAMS-FUNCAO            TO WAMT-FUNCAO.
           MOVE WAMS-FUNC-BDSCO        TO WAMT-FUNC-BDSCO.
           MOVE WAMS-AGENCIA           TO WAMT-AGENCIA.
           MOVE WAMS-CONTA             TO WAMT-CONTA.

           MOVE 'S'                    TO WAMT-FIM.

           MOVE ZEROS                  TO WAMT-COD-SQL-ERRO
                                          WAMT-COD-MSG-ERRO
                                          WAMT-ERRO.

           MOVE '0000'                 TO GFCTG2-COD-MSG.

           PERFORM 1110-OBTER-DESC-MENSAGEM.

           MOVE GFCTG3-DESC-MSG-ERRO   TO WAMT-DESC-MSG-ERRO.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    OBTER A DESCRICAO DA MENSAGEM.                              *
      ******************************************************************
      *----------------------------------------------------------------*
       1110-OBTER-DESC-MENSAGEM        SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTG3-SAIDA.

           INITIALIZE                  GFCTG3-SAIDA.

           MOVE 0100                   TO GFCTG2-LL.
           MOVE ZEROS                  TO GFCTG2-ZZ.
           MOVE WAMS-TRANSACAO         TO GFCTG2-TRANSACAO.
           MOVE WAMS-FUNCAO            TO GFCTG2-FUNCAO.
           MOVE WAMS-FUNC-BDSCO        TO GFCTG2-FUNC-BDSCO.
           MOVE 'O'                    TO GFCTG2-TIPO-PROC.
           MOVE 'N'                    TO GFCTG2-FIM.

           MOVE 'GFCT5523'             TO WRK-MODULO.

           CALL WRK-MODULO             USING GFCTG2-ENTRADA
                                             GFCTG3-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-NOME-MOD
               MOVE WRK-MSG01          TO GFCT0M-TEXTO(1:34)
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT5289'         TO GFCT0M-TRANSACAO
               MOVE 9                  TO WAMT-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG3-ERRO             EQUAL 9
               MOVE 9                  TO WAMT-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-NOME-MOD
               MOVE WRK-MSG01          TO GFCT0M-TEXTO(1:34)
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT5289'         TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG3-ERRO             EQUAL 1
               MOVE 1                  TO WAMT-ERRO
               MOVE GFCTG3-COD-SQL-ERRO
                                       TO WAMT-COD-SQL-ERRO
               MOVE GFCTG3-COD-MSG-ERRO
                                       TO WAMT-COD-MSG-ERRO
               MOVE GFCTG3-DESC-MSG-ERRO
                                       TO WAMT-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *   ROTINA PARA VERIFICACAO DA DISPONIBILIDADE DO SISTEMA        *
      ******************************************************************
      *----------------------------------------------------------------*
       1200-VERIFICAR-SIST-DISPONIVEL   SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTG1-SAIDA.

           INITIALIZE                  GFCTG1-SAIDA.

           MOVE 'GFCT'                 TO GFCTFZ-SISTEMA.
           MOVE WAMS-TRANSACAO         TO GFCTFZ-TRANSACAO.
           MOVE WAMS-FUNCAO            TO GFCTFZ-FUNCAO.
           MOVE WAMS-FUNC-BDSCO        TO GFCTFZ-FUNC-BDSCO.
           MOVE 'N'                    TO GFCTFZ-FIM.
           MOVE 'GFCT5522'             TO WRK-MODULO.

           CALL  WRK-MODULO            USING GFCTFZ-ENTRADA
                                             GFCTG1-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 9                  TO WAMT-ERRO
               MOVE WRK-MODULO         TO WRK-NOME-MOD
                                          GFCT0M-PROGRAMA
               MOVE WRK-MSG01          TO GFCT0M-TEXTO(1:34)
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT5289'         TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG1-ERRO             EQUAL 9
               IF  GFCT0M-TIPO-ACESSO  NOT EQUAL 'DB2'
                   MOVE GFCTG1-DESC-MSG-ERRO
                                       TO WRK-MSG-AUX
                   MOVE WRK-MSG-75     TO GFCT0M-TEXTO
                   MOVE 'APL'          TO GFCT0M-TIPO-ACESSO
               ELSE
                   MOVE GFCTG1-COD-SQL-ERRO
                                       TO WAMT-COD-SQL-ERRO
               END-IF
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
               MOVE 'GFCT5289'         TO GFCT0M-TRANSACAO
               MOVE GFCTG1-ERRO        TO WAMT-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTG1-CSIT-ONLINE      EQUAL 1
               MOVE WRK-MSG02          TO GFCT0M-TEXTO(1:26)
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT5289'         TO GFCT0M-TRANSACAO
               MOVE 9                  TO WAMT-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    CONSISTE A AREA DE ENTRADA.                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       1300-EFETUAR-CONSISTENCIA       SECTION.
      *----------------------------------------------------------------*

           IF (WAMS-TRANSACAO          EQUAL SPACES
                                       OR LOW-VALUES) OR
              (WAMS-FUNCAO             EQUAL SPACES
                                       OR LOW-VALUES) OR
              (WAMS-FUNC-BDSCO         EQUAL SPACES
                                       OR LOW-VALUES) OR
              (WAMS-AGENCIA            NOT NUMERIC    OR
               WAMS-AGENCIA            EQUAL ZEROS)   OR
              (WAMS-CONTA              NOT NUMERIC    OR
               WAMS-CONTA              EQUAL ZEROS)
               MOVE 1                  TO WAMT-ERRO
                                          WAMT-COD-MSG-ERRO
               MOVE ZEROS              TO WAMT-COD-SQL-ERRO
               MOVE '0001'             TO GFCTG2-COD-MSG
               PERFORM 1110-OBTER-DESC-MENSAGEM
               MOVE GFCTG3-DESC-MSG    TO WAMT-DESC-MSG-ERRO(1:70)
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    PROCESSAMENTO PRINCIPAL DO PROGRAMA                         *
      ******************************************************************
      *----------------------------------------------------------------*
       2000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 2100-ACESSAR-GFCTB0A1.

           PERFORM 2200-TRATAR-GFCTB009.

           MOVE CSERVC-TARIF      OF GFCTB009  TO WRK-AUX-5-S.
           MOVE WRK-AUX-5                      TO WAMT-COD-TARIFA.
           MOVE DINIC-ADSAO-INDVD OF GFCTB009  TO WAMT-DATA-INI-VIG.
           MOVE DFIM-ADSAO-INDVD  OF GFCTB009  TO WAMT-DATA-FIM-VIG.
           MOVE HINCL-REG         OF GFCTB009  TO WAMT-HINCL.

           PERFORM 2300-OBTER-DESC-TARIFA.

           MOVE RSERVC-TARIF-REDZD OF GFCTB0D8(1:40)
                                               TO WAMT-DESCR-TARIFA.

           PERFORM 2400-OBTER-DADOS-CLIENTE.

           MOVE GFCTHA-NOME-CLIENTE(1:40)      TO WAMT-NOME-CLI

           PERFORM 2600-TRATAR-GFCTB0M2.

           PERFORM 2700-OBTER-TP-CTA-PESSOA.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA DE ACESSO A TABELA GFCTB0A1                          *
      ******************************************************************
      *----------------------------------------------------------------*
       2100-ACESSAR-GFCTB0A1           SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT'                 TO CSIST-PRINC      OF GFCTB0A1.

           EXEC SQL
             SELECT
                   DPROCM_ATUAL
             INTO
                  :GFCTB0A1.DPROCM-ATUAL
             FROM  DB2PRD.PARM_DATA_PROCM
             WHERE
                   CSIST_PRINC         = :GFCTB0A1.CSIST-PRINC
           END-EXEC.

           IF  (SQLCODE                NOT EQUAL ZEROS) OR
               (SQLWARN0               EQUAL 'W')
                MOVE '0010'            TO GFCTG2-COD-MSG
                PERFORM 1110-OBTER-DESC-MENSAGEM
                INITIALIZE             GFCT0M-ERRO-SQL
                MOVE ' - GFCTB0A1'     TO WRK-NOME-TAB
                MOVE 'GFCT5289'        TO GFCT0M-PROGRAMA
                MOVE 'DB2'             TO GFCT0M-TIPO-ACESSO
                MOVE 'PARM_DATA_PROCM' TO GFCT0M-NOME-TAB
                MOVE 'SELECT'          TO GFCT0M-COMANDO-SQL
                MOVE '0001'            TO GFCT0M-LOCAL
                MOVE SQLCA             TO GFCT0M-SQLCA-AREA
                MOVE 9                 TO WAMT-ERRO
                MOVE SQLCODE           TO WRK-AUXILIAR-4
                MOVE WRK-AUX-4         TO WAMT-COD-SQL-ERRO
                MOVE SPACES            TO WAMT-DESC-MSG-ERRO
                MOVE 0010              TO WAMT-COD-MSG-ERRO
                PERFORM 2110-CONCATENAR-MSG
                PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 1                  TO WAMT-ERRO
               MOVE ZEROS              TO WAMT-COD-SQL-ERRO
               MOVE 1455               TO WAMT-COD-MSG-ERRO
               MOVE '1455'             TO GFCTG2-COD-MSG
               PERFORM 1110-OBTER-DESC-MENSAGEM
               MOVE GFCTG3-DESC-MSG    TO WAMT-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    CONCATENA MENSAGEM DE SAIDA.                                *
      ******************************************************************
      *----------------------------------------------------------------*
       2110-CONCATENAR-MSG             SECTION.
      *----------------------------------------------------------------*

           STRING GFCTG3-DESC-MSG      WRK-NOME-TAB
           DELIMITED BY '  '           INTO WAMT-DESC-MSG-ERRO.

      *----------------------------------------------------------------*
       2110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA TRATAR-GFCTB009                                      *
      ******************************************************************
      *----------------------------------------------------------------*
       2200-TRATAR-GFCTB009            SECTION.
      *----------------------------------------------------------------*

           PERFORM 2210-ABRIR-CSR02

           PERFORM 2220-LER-CSR02

           PERFORM 2230-FECHAR-CSR02.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ABRIR CURSOR DA TABELA GFCTB009                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       2210-ABRIR-CSR02                SECTION.
      *----------------------------------------------------------------*

           MOVE WAMS-AGENCIA           TO CJUNC-DEPDC      OF GFCTB009
           MOVE WAMS-CONTA             TO CCTA-CLI         OF GFCTB009
           MOVE ZEROS                  TO CINDCD-EXCL-REG  OF GFCTB009

           PERFORM 2211-CALCULAR-DATA
           MOVE WRK-DATA-DB2           TO DFIM-ADSAO-INDVD OF GFCTB009

           EXEC SQL
               OPEN CSR02-GFCTB009
           END-EXEC.

           IF  (SQLCODE                NOT EQUAL ZEROS )OR
               (SQLWARN0               EQUAL 'W')
                MOVE '0010'            TO GFCTG2-COD-MSG
                PERFORM 1110-OBTER-DESC-MENSAGEM
                INITIALIZE             GFCT0M-ERRO-SQL
                MOVE ' - GFCTB009'     TO WRK-NOME-TAB
                MOVE 'GFCT5289'        TO GFCT0M-PROGRAMA
                MOVE 'DB2'             TO GFCT0M-TIPO-ACESSO
                MOVE 'TADSAO_INDVD_CLULR'
                                       TO GFCT0M-NOME-TAB
                MOVE 'OPEN'            TO GFCT0M-COMANDO-SQL
                MOVE '0002'            TO GFCT0M-LOCAL
                MOVE SQLCA             TO GFCT0M-SQLCA-AREA
                MOVE 9                 TO WAMT-ERRO
                MOVE SQLCODE           TO WRK-AUXILIAR-4
                MOVE WRK-AUX-4         TO WAMT-COD-SQL-ERRO
                MOVE SPACES            TO WAMT-DESC-MSG-ERRO
                MOVE 0010              TO WAMT-COD-MSG-ERRO
                PERFORM 2110-CONCATENAR-MSG
                PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA CALCULAR-DATA: ULTIMO DIA DO MES CORRENTE
      ******************************************************************
      *----------------------------------------------------------------*
       2211-CALCULAR-DATA              SECTION.
      *----------------------------------------------------------------*

           MOVE DPROCM-ATUAL OF GFCTB0A1
                                       TO WRK-DATA-DB2
           MOVE WRK-MES-DB2            TO WRK-MES-COMP
           MOVE WRK-ANO-DB2            TO WRK-ANO-COMP

           IF  WRK-MES-COMP EQUAL 12
               SUBTRACT 12             FROM WRK-MES-COMP
               ADD 1                   TO WRK-ANO-COMP
           END-IF.

           ADD 1                       TO WRK-MES-COMP
           MOVE 1                      TO WRK-DIA-AMD
           MOVE WRK-MES-COMP           TO WRK-MES-AMD
           MOVE WRK-ANO-COMP           TO WRK-ANO-AMD

           MOVE WRK-DATA-AMD           TO WRK-P0025-DATA-ENT
           MOVE -1                     TO WRK-P0025-DIAS
           MOVE ZEROS                  TO WRK-P0025-DATA-SAI

           CALL 'POOL0025'          USING WRK-P0025-DATA-ENT
                                          WRK-P0025-DIAS
                                          WRK-P0025-DATA-SAI

           MOVE WRK-P0025-DATA-SAI     TO WRK-DATA-AMD
           MOVE WRK-DIA-AMD            TO WRK-DIA-DB2
           MOVE WRK-MES-AMD            TO WRK-MES-DB2
           MOVE WRK-ANO-AMD            TO WRK-ANO-DB2.

      *----------------------------------------------------------------*
       2211-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * LER CURSOR DA TABELA GFCTB009                                  *
      ******************************************************************
      *----------------------------------------------------------------*
       2220-LER-CSR02                  SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             FETCH CSR02-GFCTB009   INTO
                   :GFCTB009.CSERVC-TARIF ,
                   :GFCTB009.HINCL-REG ,
                   :GFCTB009.DINIC-ADSAO-INDVD ,
                   :GFCTB009.DFIM-ADSAO-INDVD
           END-EXEC.

           IF  (SQLCODE                NOT EQUAL ZEROS AND +100) OR
               (SQLWARN0               EQUAL 'W')
                MOVE '0010'            TO GFCTG2-COD-MSG
                PERFORM 1110-OBTER-DESC-MENSAGEM
                INITIALIZE             GFCT0M-ERRO-SQL
                MOVE ' - GFCTB009'     TO WRK-NOME-TAB
                MOVE 'GFCT5289'        TO GFCT0M-PROGRAMA
                MOVE 'DB2'             TO GFCT0M-TIPO-ACESSO
                MOVE 'TADSAO_INDVD_CLULR'
                                       TO GFCT0M-NOME-TAB
                MOVE 'FETCH'           TO GFCT0M-COMANDO-SQL
                MOVE '0003'            TO GFCT0M-LOCAL
                MOVE SQLCA             TO GFCT0M-SQLCA-AREA
                MOVE 9                 TO WAMT-ERRO
                MOVE SQLCODE           TO WRK-AUXILIAR-4
                MOVE WRK-AUX-4         TO WAMT-COD-SQL-ERRO
                MOVE SPACES            TO WAMT-DESC-MSG-ERRO
                MOVE 0010              TO WAMT-COD-MSG-ERRO
                PERFORM 2110-CONCATENAR-MSG
                PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 1                  TO WAMT-ERRO
               MOVE ZEROS              TO WAMT-COD-SQL-ERRO
               MOVE 1672               TO WAMT-COD-MSG-ERRO
               MOVE '1672'             TO GFCTG2-COD-MSG
               PERFORM 1110-OBTER-DESC-MENSAGEM
               MOVE GFCTG3-DESC-MSG    TO WAMT-DESC-MSG-ERRO
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2220-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    FECHAR CURSOR DA TABELA GFCTB009                            *
      ******************************************************************
      *----------------------------------------------------------------*
       2230-FECHAR-CSR02               SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               CLOSE CSR02-GFCTB009
           END-EXEC.

           IF  (SQLCODE                NOT EQUAL ZEROS )OR
               (SQLWARN0               EQUAL 'W')
                MOVE '0010'            TO GFCTG2-COD-MSG
                PERFORM 1110-OBTER-DESC-MENSAGEM
                INITIALIZE             GFCT0M-ERRO-SQL
                MOVE ' - GFCTB009'     TO WRK-NOME-TAB
                MOVE 'GFCT5289'        TO GFCT0M-PROGRAMA
                MOVE 'DB2'             TO GFCT0M-TIPO-ACESSO
                MOVE 'TADSAO_INDVD_CLULR'
                                       TO GFCT0M-NOME-TAB
                MOVE 'CLOSE'           TO GFCT0M-COMANDO-SQL
                MOVE '0004'            TO GFCT0M-LOCAL
                MOVE SQLCA             TO GFCT0M-SQLCA-AREA
                MOVE 9                 TO WAMT-ERRO
                MOVE SQLCODE           TO WRK-AUXILIAR-4
                MOVE WRK-AUX-4         TO WAMT-COD-SQL-ERRO
                MOVE SPACES            TO WAMT-DESC-MSG-ERRO
                MOVE 0010              TO WAMT-COD-MSG-ERRO
                PERFORM 2110-CONCATENAR-MSG
                PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2230-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA OBTER A DESCRICAO DA TARIFA                          *
      ******************************************************************
      *----------------------------------------------------------------*
       2300-OBTER-DESC-TARIFA          SECTION.
      *----------------------------------------------------------------*

           MOVE CSERVC-TARIF OF GFCTB009 TO CSERVC-TARIF OF GFCTB0D8.

           EXEC SQL
                SELECT
                   RSERVC_TARIF_REDZD
                INTO
                   :GFCTB0D8.RSERVC-TARIF-REDZD
                FROM   DB2PRD.SERVC_TARIF_PRINC
                WHERE
                       CSERVC_TARIF       = :GFCTB0D8.CSERVC-TARIF
           END-EXEC.

           IF  (SQLCODE                NOT EQUAL ZEROS AND +100)OR
               (SQLWARN0               EQUAL 'W')
                MOVE '0010'            TO GFCTG2-COD-MSG
                PERFORM 1110-OBTER-DESC-MENSAGEM
                INITIALIZE             GFCT0M-ERRO-SQL
                MOVE ' - GFCTB0D8'     TO WRK-NOME-TAB
                MOVE 'GFCT5289'        TO GFCT0M-PROGRAMA
                MOVE 'DB2'             TO GFCT0M-TIPO-ACESSO
                MOVE 'SERVC_TARIF_PRNC'
                                       TO GFCT0M-NOME-TAB
                MOVE 'SELECT'          TO GFCT0M-COMANDO-SQL
                MOVE '0005'            TO GFCT0M-LOCAL
                MOVE SQLCA             TO GFCT0M-SQLCA-AREA
                MOVE 9                 TO WAMT-ERRO
                MOVE SQLCODE           TO WRK-AUXILIAR-4
                MOVE WRK-AUX-4         TO WAMT-COD-SQL-ERRO
                MOVE SPACES            TO WAMT-DESC-MSG-ERRO
                MOVE 0010              TO WAMT-COD-MSG-ERRO
                PERFORM 2110-CONCATENAR-MSG
                PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 1                  TO WAMT-ERRO
               MOVE ZEROS              TO WAMT-COD-SQL-ERRO
               MOVE 1284               TO WAMT-COD-MSG-ERRO
               MOVE '1284'             TO GFCTG2-COD-MSG
               PERFORM 1110-OBTER-DESC-MENSAGEM
               MOVE GFCTG3-DESC-MSG    TO WAMT-DESC-MSG-ERRO(1:70)
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA OBTER O CPJ / CNPJ                                   *
      ******************************************************************
      *----------------------------------------------------------------*
       2400-OBTER-DADOS-CLIENTE        SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTG0-ENTRADA.
           MOVE 100                    TO GFCTG0-LL.
           MOVE ZEROS                  TO GFCTG0-ZZ.
           MOVE WAMS-TRANSACAO         TO GFCTG0-TRANSACAO.
           MOVE WAMS-FUNCAO            TO GFCTG0-FUNCAO.
           MOVE WAMS-FUNC-BDSCO        TO GFCTG0-FUNC-BDSCO.
           MOVE WAMS-AGENCIA           TO GFCTG0-COD-DEPDC.
           MOVE WAMS-CONTA             TO GFCTG0-CCTA-CLI.
           MOVE 'N'                    TO GFCTG0-FIM.

           MOVE SPACES                 TO GFCTHA-SAIDA
                                          GFCT0M-AREA-ERROS.

           INITIALIZE  GFCTHA-SAIDA
                       GFCT0M-AREA-ERROS.

           MOVE 'GFCT5054'             TO WRK-MODULO.

           CALL WRK-MODULO             USING GFCTG0-ENTRADA
                                             GFCTHA-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 9                  TO WAMT-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-MSG05-MODULO
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT5289'         TO GFCT0M-TRANSACAO
               PERFORM 3000-FINALIZAR
           END-IF.

           IF  GFCTHA-ERRO             NOT EQUAL ZEROS
               MOVE 'S'                TO WAMT-FIM
               MOVE GFCTHA-ERRO        TO WAMT-ERRO
               MOVE GFCTHA-COD-SQL-ERRO
                                       TO WAMT-COD-SQL-ERRO
               MOVE GFCTHA-COD-MSG-ERRO
                                       TO WAMT-COD-MSG-ERRO

               IF  GFCTHA-ERRO         EQUAL 1
                   MOVE GFCTHA-DESC-MSG-ERRO
                                       TO WAMT-DESC-MSG-ERRO
               ELSE
                   STRING 'GFCT5054-' GFCTHA-DESC-MSG-ERRO
                   DELIMITED BY SIZE   INTO WAMT-DESC-MSG-ERRO
               END-IF
               PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA OBTER DADOS DAS OPERADORAS DE CELULAR                *
      ******************************************************************
      *----------------------------------------------------------------*
       2600-TRATAR-GFCTB0M2            SECTION.
      *----------------------------------------------------------------*

           PERFORM 2610-ABRIR-CSR01

           PERFORM 2620-LER-CSR01

           PERFORM 2630-FECHAR-CSR01.

      *----------------------------------------------------------------*
       2600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *ABRIR CURSOR DA TABELA GFCTB0M2                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       2610-ABRIR-CSR01                SECTION.
      *----------------------------------------------------------------*

           MOVE CJUNC-DEPDC            OF GFCTB009
                                       TO CAG-BCRIA        OF GFCTB0M2
           MOVE CCTA-CLI               OF GFCTB009
                                       TO CCTA-CLI         OF GFCTB0M2
           MOVE CSERVC-TARIF           OF GFCTB009
                                       TO CSERVC-TARIF     OF GFCTB0M2
           MOVE HINCL-REG              OF GFCTB009
                                       TO HINCL-REG        OF GFCTB0M2

           EXEC SQL
               OPEN CSR01-GFCTB0M2
           END-EXEC.

           IF  (SQLCODE                NOT EQUAL ZEROS )OR
               (SQLWARN0               EQUAL 'W')
                MOVE '0010'            TO GFCTG2-COD-MSG
                PERFORM 1110-OBTER-DESC-MENSAGEM
                INITIALIZE             GFCT0M-ERRO-SQL
                MOVE ' - GFCTB0M2'     TO WRK-NOME-TAB
                MOVE 'GFCT5289'        TO GFCT0M-PROGRAMA
                MOVE 'DB2'             TO GFCT0M-TIPO-ACESSO
                MOVE 'TADSAO_INDVD_CLULR'
                                       TO GFCT0M-NOME-TAB
                MOVE 'OPEN'            TO GFCT0M-COMANDO-SQL
                MOVE '0006'            TO GFCT0M-LOCAL
                MOVE SQLCA             TO GFCT0M-SQLCA-AREA
                MOVE 9                 TO WAMT-ERRO
                MOVE SQLCODE           TO WRK-AUXILIAR-4
                MOVE WRK-AUX-4         TO WAMT-COD-SQL-ERRO
                MOVE SPACES            TO WAMT-DESC-MSG-ERRO
                MOVE 0010              TO WAMT-COD-MSG-ERRO
                PERFORM 2110-CONCATENAR-MSG
                PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2610-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * LER CURSOR DA TABELA GFCTB0M2                                  *
      ******************************************************************
      *----------------------------------------------------------------*
       2620-LER-CSR01                  SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             FETCH CSR01-GFCTB0M2   INTO
                   :GFCTB0M2.NSEQ-REG-CLULR,
                   :GFCTB0M2.COPER-FONE-MOVEL,
                   :GFCTB0M2.CDDD-FONE-MOVEL,
022012             :GFCTB0M2.NLIN-TFONI
           END-EXEC.

           IF  (SQLCODE                NOT EQUAL ZEROS AND +100)OR
               (SQLWARN0               EQUAL 'W')
                MOVE '0010'            TO GFCTG2-COD-MSG
                PERFORM 1110-OBTER-DESC-MENSAGEM
                INITIALIZE             GFCT0M-ERRO-SQL
                MOVE ' - GFCTB0M2'     TO WRK-NOME-TAB
                MOVE 'GFCT5289'        TO GFCT0M-PROGRAMA
                MOVE 'DB2'             TO GFCT0M-TIPO-ACESSO
                MOVE 'TADSAO_INDVD_CLULR'
                                       TO GFCT0M-NOME-TAB
                MOVE 'FETCH'           TO GFCT0M-COMANDO-SQL
                MOVE '0007'            TO GFCT0M-LOCAL
                MOVE SQLCA             TO GFCT0M-SQLCA-AREA
                MOVE 9                 TO WAMT-ERRO
                MOVE SQLCODE           TO WRK-AUXILIAR-4
                MOVE WRK-AUX-4         TO WAMT-COD-SQL-ERRO
                MOVE SPACES            TO WAMT-DESC-MSG-ERRO
                MOVE 0010              TO WAMT-COD-MSG-ERRO
                PERFORM 2110-CONCATENAR-MSG
                PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE ZEROS              TO WAMT-CD-OPERADORA
                                          WAMT-CD-DDD
                                          WAMT-NR-TELEFONE
               MOVE SPACES             TO WAMT-DS-OPERADORA
           ELSE
              MOVE NSEQ-REG-CLULR      OF GFCTB0M2
                                       TO WRK-AUX-5-S
              MOVE WRK-AUX-5           TO WAMT-SEQCEL

              MOVE COPER-FONE-MOVEL    OF GFCTB0M2
                                       TO WRK-AUX-9-S
              MOVE WRK-AUX-9           TO WAMT-CD-OPERADORA
              MOVE CDDD-FONE-MOVEL     OF GFCTB0M2
                                       TO WRK-AUX-3-S
              MOVE WRK-AUX-3           TO WAMT-CD-DDD
022012        MOVE NLIN-TFONI          OF GFCTB0M2
022012                                 TO WRK-AUX-11-S
022012        MOVE WRK-AUX-11          TO WAMT-NR-TELEFONE
              PERFORM 2621-OBTER-DESC-OPERAD
           END-IF.

      *----------------------------------------------------------------*
       2620-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ACESSAR DESCRICAO DA OPERADORA                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       2621-OBTER-DESC-OPERAD          SECTION.
      *----------------------------------------------------------------*

           MOVE COPER-FONE-MOVEL       OF GFCTB0M2
                                       TO COPER-FONE-MOVEL  OF GFCTB0M4
           MOVE CDDD-FONE-MOVEL        OF GFCTB0M2
                                       TO CDDD-FONE-MOVEL   OF GFCTB0M4

           EXEC SQL
                SELECT
                   ROPER_FONE_MOVEL
                INTO
                   :GFCTB0M4.ROPER-FONE-MOVEL
                FROM
                   DB2PRD.TOPER_FONE_CREDT
                WHERE
                    COPER_FONE_MOVEL   = :GFCTB0M4.COPER-FONE-MOVEL
                AND CDDD_FONE_MOVEL    = :GFCTB0M4.CDDD-FONE-MOVEL
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
                MOVE '0010'            TO GFCTG2-COD-MSG
                PERFORM 1110-OBTER-DESC-MENSAGEM
                INITIALIZE             GFCT0M-ERRO-SQL
                MOVE ' - GFCTB0M4'     TO WRK-NOME-TAB
                MOVE 'GFCT0337'        TO GFCT0M-PROGRAMA
                MOVE 'DB2'             TO GFCT0M-TIPO-ACESSO
                MOVE 'TOPER_FONE_CREDT'
                                       TO GFCT0M-NOME-TAB
                MOVE 'SELECT'          TO GFCT0M-COMANDO-SQL
                MOVE '0008'            TO GFCT0M-LOCAL
                MOVE SQLCA             TO GFCT0M-SQLCA-AREA
                MOVE 9                 TO WAMT-ERRO
                MOVE SQLCODE           TO WRK-AUXILIAR-4
                MOVE WRK-AUX-4         TO WAMT-COD-SQL-ERRO
                MOVE SPACES            TO WAMT-DESC-MSG-ERRO
                MOVE 0010              TO WAMT-COD-MSG-ERRO
                PERFORM 2110-CONCATENAR-MSG
                PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE SPACES             TO WAMT-DS-OPERADORA
           ELSE
               MOVE ROPER-FONE-MOVEL   OF GFCTB0M4
                                       TO WAMT-DS-OPERADORA
           END-IF.

      *----------------------------------------------------------------*
       2621-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *FECHAR CURSOR 1                                                 *
      ******************************************************************
      *----------------------------------------------------------------*
       2630-FECHAR-CSR01               SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               CLOSE CSR01-GFCTB0M2
           END-EXEC.

           IF  (SQLCODE                NOT EQUAL ZEROS )OR
               (SQLWARN0               EQUAL 'W')
                MOVE '0010'            TO GFCTG2-COD-MSG
                PERFORM 1110-OBTER-DESC-MENSAGEM
                INITIALIZE             GFCT0M-ERRO-SQL
                MOVE ' - GFCTB0M2'     TO WRK-NOME-TAB
                MOVE 'GFCT5289'        TO GFCT0M-PROGRAMA
                MOVE 'DB2'             TO GFCT0M-TIPO-ACESSO
                MOVE 'TADSAO_INDVD_CLULR'
                                       TO GFCT0M-NOME-TAB
                MOVE 'CLOSE'           TO GFCT0M-COMANDO-SQL
                MOVE '0009'            TO GFCT0M-LOCAL
                MOVE SQLCA             TO GFCT0M-SQLCA-AREA
                MOVE 9                 TO WAMT-ERRO
                MOVE SQLCODE           TO WRK-AUXILIAR-4
                MOVE WRK-AUX-4         TO WAMT-COD-SQL-ERRO
                MOVE SPACES            TO WAMT-DESC-MSG-ERRO
                MOVE 0010              TO WAMT-COD-MSG-ERRO
                PERFORM 2110-CONCATENAR-MSG
                PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2630-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA OBTER TIPO DE CONTA E TIPO DE PESSOA                 *
      ******************************************************************
      *----------------------------------------------------------------*
       2700-OBTER-TP-CTA-PESSOA        SECTION.
      *----------------------------------------------------------------*

           PERFORM 2710-ACESSAR-CLIEV007

           IF  SQLCODE                 EQUAL +100

               PERFORM 2720-ACESSAR-CLIEV008

               IF  SQLCODE             EQUAL +100

                   MOVE 1              TO WAMT-ERRO
                   MOVE ZEROS          TO WAMT-COD-SQL-ERRO
                   MOVE 0711           TO WAMT-COD-MSG-ERRO
                   MOVE '0711'         TO GFCTG2-COD-MSG
                   PERFORM 1110-OBTER-DESC-MENSAGEM
                   MOVE GFCTG3-DESC-MSG
                                       TO WAMT-DESC-MSG-ERRO(1:70)
                   PERFORM 3000-FINALIZAR
               ELSE
                   MOVE 1              TO WAMT-TIPO-CONTA
                   MOVE CID-CLI        OF CLIEV008
                                       TO CID-CLI          OF CLIEV004
                   PERFORM 2730-ACESSAR-CLIEV004
               END-IF
           ELSE
               MOVE 2                  TO WAMT-TIPO-CONTA
               MOVE CID-CLI            OF CLIEV007
                                       TO CID-CLI          OF CLIEV004
               PERFORM 2730-ACESSAR-CLIEV004
           END-IF.

      *----------------------------------------------------------------*
       2700-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA ACESSAR CLIEV007                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       2710-ACESSAR-CLIEV007           SECTION.
      *----------------------------------------------------------------*

           MOVE WAMS-AGENCIA           TO CJUNC-DEPDC      OF CLIEV007.
           MOVE WAMS-CONTA             TO CCTA-CLI         OF CLIEV007.

           EXEC SQL
             SELECT
                   CID_CLI
             INTO
                   :CLIEV007.CID-CLI
             FROM   DB2PRD.V01CTA_POUPANCA
             WHERE
                   CJUNC_DEPDC        = :CLIEV007.CJUNC-DEPDC AND
                   CCTA_CLI           = :CLIEV007.CCTA-CLI
           END-EXEC.

           IF  (SQLCODE                NOT EQUAL ZEROS AND +100)OR
               (SQLWARN0               EQUAL 'W')
                MOVE '0010'            TO GFCTG2-COD-MSG
                PERFORM 1110-OBTER-DESC-MENSAGEM
                INITIALIZE             GFCT0M-ERRO-SQL
                MOVE ' - CLIEV007'     TO WRK-NOME-TAB
                MOVE 'GFCT5289'        TO GFCT0M-PROGRAMA
                MOVE 'DB2'             TO GFCT0M-TIPO-ACESSO
                MOVE 'V01CTA_POUPANCA' TO GFCT0M-NOME-TAB
                MOVE 'SELECT'          TO GFCT0M-COMANDO-SQL
                MOVE '0010'            TO GFCT0M-LOCAL
                MOVE SQLCA             TO GFCT0M-SQLCA-AREA
                MOVE 9                 TO WAMT-ERRO
                MOVE SQLCODE           TO WRK-AUXILIAR-4
                MOVE WRK-AUX-4         TO WAMT-COD-SQL-ERRO
                MOVE SPACES            TO WAMT-DESC-MSG-ERRO
                MOVE 0010              TO WAMT-COD-MSG-ERRO
                PERFORM 2110-CONCATENAR-MSG
                PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2710-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA ACESSAR CLIEV008                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       2720-ACESSAR-CLIEV008           SECTION.
      *----------------------------------------------------------------*

           MOVE WAMS-AGENCIA           TO CJUNC-DEPDC      OF CLIEV008.
           MOVE WAMS-CONTA             TO CCTA-CLI         OF CLIEV008.

           EXEC SQL
             SELECT
                   CID_CLI
             INTO
                   :CLIEV008.CID-CLI
             FROM   DB2PRD.V01CTA_CORRENTE
             WHERE
                   CJUNC_DEPDC         = :CLIEV008.CJUNC-DEPDC AND
                   CCTA_CLI            = :CLIEV008.CCTA-CLI
           END-EXEC.

           IF  (SQLCODE                NOT EQUAL ZEROS AND +100)OR
               (SQLWARN0               EQUAL 'W')
                MOVE '0010'            TO GFCTG2-COD-MSG
                PERFORM 1110-OBTER-DESC-MENSAGEM
                INITIALIZE             GFCT0M-ERRO-SQL
                MOVE ' - CLIEV008'     TO WRK-NOME-TAB
                MOVE 'GFCT5289'        TO GFCT0M-PROGRAMA
                MOVE 'DB2'             TO GFCT0M-TIPO-ACESSO
                MOVE 'V01CTA_CORRENTE' TO GFCT0M-NOME-TAB
                MOVE 'SELECT'          TO GFCT0M-COMANDO-SQL
                MOVE '0011'            TO GFCT0M-LOCAL
                MOVE SQLCA             TO GFCT0M-SQLCA-AREA
                MOVE 9                 TO WAMT-ERRO
                MOVE SQLCODE           TO WRK-AUXILIAR-4
                MOVE WRK-AUX-4         TO WAMT-COD-SQL-ERRO
                MOVE SPACES            TO WAMT-DESC-MSG-ERRO
                MOVE 0010              TO WAMT-COD-MSG-ERRO
                PERFORM 2110-CONCATENAR-MSG
                PERFORM 3000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2720-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA ACESSAR CLIEV004                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       2730-ACESSAR-CLIEV004           SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             SELECT
                    CFLIAL_CGC,
ST25X6*            Inclusão do novo campo -ST no SELECT
                    CFLIAL_CGC_ST
             INTO
                    :CLIEV004.CFLIAL-CGC,
ST25X6*            Inclusão do novo host -ST no INTO
                    :CLIEV004.CFLIAL-CGC-ST
             FROM   DB2PRD.V01CLIENTE_AGENCIA
             WHERE
                   CID_CLI            = :CLIEV004.CID-CLI
           END-EXEC.

           IF  (SQLCODE                NOT EQUAL ZEROS AND +100)OR
               (SQLWARN0               EQUAL 'W')
                MOVE '0010'            TO GFCTG2-COD-MSG
                PERFORM 1110-OBTER-DESC-MENSAGEM
                INITIALIZE             GFCT0M-ERRO-SQL
                MOVE ' - CLIEV004'     TO WRK-NOME-TAB
                MOVE 'GFCT5289'        TO GFCT0M-PROGRAMA
                MOVE 'DB2'             TO GFCT0M-TIPO-ACESSO
                MOVE 'V01CLIENTE_AGENCIA'
                                       TO GFCT0M-NOME-TAB
                MOVE 'SELECT'          TO GFCT0M-COMANDO-SQL
                MOVE '0012'            TO GFCT0M-LOCAL
                MOVE SQLCA             TO GFCT0M-SQLCA-AREA
                MOVE 9                 TO WAMT-ERRO
                MOVE SQLCODE           TO WRK-AUXILIAR-4
                MOVE WRK-AUX-4         TO WAMT-COD-SQL-ERRO
                MOVE SPACES            TO WAMT-DESC-MSG-ERRO
                MOVE 0010              TO WAMT-COD-MSG-ERRO
                PERFORM 2110-CONCATENAR-MSG
                PERFORM 3000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 1                  TO WAMT-ERRO
               MOVE ZEROS              TO WAMT-COD-SQL-ERRO
               MOVE 0710               TO WAMT-COD-MSG-ERRO
               MOVE '0710'             TO GFCTG2-COD-MSG
               PERFORM 1110-OBTER-DESC-MENSAGEM
               MOVE GFCTG3-DESC-MSG    TO WAMT-DESC-MSG-ERRO(1:70)
           ELSE
ST25X6*        Utilização do novo campo -ST na verificação do tipo de pessoa
               IF  CFLIAL-CGC-ST OF CLIEV004
                                       EQUAL ZEROS
               OR  CFLIAL-CGC-ST OF CLIEV004
                                       EQUAL SPACES
                   MOVE 'F'            TO WAMT-TIPO-PESSOA
               ELSE
                   MOVE 'J'            TO WAMT-TIPO-PESSOA
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2730-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA PARA FINALIZACAO DO PROCESSAMENTO.                   *
      ******************************************************************
      *----------------------------------------------------------------*
       3000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 3100-RETORNAR-CHAMADOR.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      *    ROTINA PARA RETORNO AO CHAMADOR                             *
      ******************************************************************
      *----------------------------------------------------------------*
       3100-RETORNAR-CHAMADOR          SECTION.
      *----------------------------------------------------------------*

           GOBACK.

      *----------------------------------------------------------------*
       3100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
