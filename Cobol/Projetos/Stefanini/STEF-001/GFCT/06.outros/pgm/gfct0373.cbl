      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT0373.
       AUTHOR. JEAN FRETTA PEREIRA.
      *================================================================*
      *                   C P M   S I S T E M A S                      *
      *----------------------------------------------------------------*
      *                                                                *
      *    PROGRAMA     : GFCT0373                                     *
      *    PROGRAMADOR  : JEAN FRETTA PEREIRA         - CPM/FPOLIS     *
      *    ANALISTA CPM : SILVANA F. SOUZA MACHADO    - CPM/FPOLIS     *
      *    ANALISTA     : MARCELO         - PROCWORK  - GRUPO 50       *
      *    DATA         : 26/10/2005                                   *
      *                                                                *
      *    OBJETIVO     :                                              *
      *      SOLICITACAO DE ESTORNO POR AGRUPAMENTO.                   *
      *                                                                *
      *    BANCO DE DADOS:                                             *
      *     DB2                                                        *
      *      TABLE                                 INCLUDE/BOOK        *
      *      DB2PRD.PARM_DATA_PROCM                  GFCTB0A1          *
      *      DB2PRD.PARM_SERVC_TARIF                 GFCTB0A2          *
      *      DB2PRD.PDIDO_ESTRN_AG                   GFCTB0A3          *
      *      DB2PRD.PDIDO_ESTRN_CLI                  GFCTB0A4          *
      *      DB2PRD.PDIDO_ESTRN_EMPR                 GFCTB0A6          *
      *      DB2PRD.PDIDO_ESTRN_GRP                  GFCTB0A7          *
      *      DB2PRD.PDIDO_ESTRN_PAB                  GFCTB0A8          *
      *      DB2PRD.PDIDO_ESTRN_PSTAL                GFCTB0A9          *
      *      DB2PRD.PDIDO_ESTRN_SGMTO                GFCTB0B0          *
      *                                                                *
      *    MODULOS CHAMADOS:                                           *
      *      GFCT5000 - OBTER DESCRICAO DE MENSAGENS                   *
      *      GFCT5500 - VERIFICAR ON-LINE                              *
      *      GFCT5060 - OBTER NOME DO CLIENTE POR AGENCIA/CONTA        *
      *      GFCT5110 - OBTER NOME DO CLIENTE POR CPF/CNPJ             *
      *      SENH0315 - OBTER DEPENDENCIA                              *
      *      MESU9015 - CONSISTIR AGENCIA                              *
      *      MESU9018 - CONSISTIR PAB                                  *
      *      POOL0081 - ALOCAR DINAMICAMENTE UM MODULO                 *
      *      POOL0110 - CONSISTIR CPF/CNPJ                             *
      *      POOL7600 - OBTER DATA E HORA DO SISTEMA                   *
      *                                                                *
      *    NAVEGACAO:                                                  *
      *      CHAMADO POR: GFCT7372                                     *
      *                                                                *
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
       01  FILLER                      PIC X(32)           VALUE
           '*  INICIO DA WORKING GFCT0373  *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*          AUXILIARES          *'.
      *----------------------------------------------------------------*

       01  WRK-FIM-CURSOR-GFCTB0A7     PIC X(01)           VALUE SPACES.
       01  WRK-ACHOU-DUPLICIDADE       PIC X(01)           VALUE SPACES.

       01  WRK-DATA-DEBITO             PIC 9(08)           VALUE ZEROS.
       01  WRK-DATA-PROCM              PIC 9(08)           VALUE ZEROS.
       01  WRK-MODULO                  PIC X(08)           VALUE SPACES.
       01  WRK-FUNC-BDSCO              PIC 9(07)           VALUE ZEROS.
       01  WRK-HORA                    PIC X(26)           VALUE SPACES.

       01  WRK-MSG-TAB.
           03  FILLER                  PIC X(03)           VALUE ' - '.
           03  WRK-NOME-TAB            PIC X(08)           VALUE SPACES.

       01  WRK-MSG05.
           03  FILLER                  PIC X(26)           VALUE
           'ERRO NA CHAMADA DO MODULO '.
           03  WRK-NOME-MODULO         PIC X(08)           VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*   INDICADORES DE NULIDADE    *'.
      *----------------------------------------------------------------*

       01  WRK-CSGMTO-GSTAO-TARIF-NULL PIC S9(04) COMP     VALUE +0.

ST2506*----------------------------------------------------------------*
ST2506*01  FILLER                      PIC X(32)           VALUE
ST2506*    '*     AREA PARA A POOL0110     *'.
ST2506*----------------------------------------------------------------*
ST2506*
ST2506*01  WRK-CPF-CGC.
ST2506*    03  WRK-CPF                 PIC 9(09)           VALUE ZEROS.
ST2506*    03  WRK-FILIAL              PIC 9(05)           VALUE ZEROS.
ST2506*01  WRK-CONTROLE                PIC X(02)           VALUE SPACES.
ST2506*----------------------------------------------------------------*
ST2506 01  FILLER                      PIC  X(050)         VALUE
ST2506    '*** AREAS DA BRAD2000 ***'.
ST2506*----------------------------------------------------------------*
ST2506
ST2506 01  WRK-BRAD2000                 PIC  X(08)  VALUE
ST2506     'BRAD2000'.
ST2506 
ST2506 01 WRK-AREA-BRAD2000.
ST2506    05 WRK-CAD-CPFCGC.
ST2506       10 WRK-CAD-NUMERO         PIC X(09)           VALUE SPACES.
ST2506       10 WRK-CAD-FILIAL         PIC X(04)           VALUE SPACES.        
ST2506 77  WRK-CAD-CONTROLE            PIC 9(02)           VALUE ZEROS.
ST2506 01  WRK-9-X-2.
ST2506     05 WRK-9-02                 PIC 9(02)           VALUE ZEROS.
ST2506*------------------------------------*----------------------------*
ST2506 01 FILLER                       PIC  X(051) VALUE
ST2506       '* AREA PARA MODULO GFCT20PJ *'.
ST2506*---------------------------------------------------------------*
ST2506 01 WRK-GFCT20PJ                 PIC X(08)   VALUE
ST2506       'GFCT20PJ'.
ST2506
ST2506 01 WRK-AREA-GFCT20PJ.
ST2506 COPY 'GFCTW2PJ'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*     AREA PARA A POOL1570     *'.
      *----------------------------------------------------------------*

       01  WRK-AREA-1570.
           03  WRK-DATA                PIC X(10)           VALUE SPACES.
           03  WRK-FORMATO-DATA        PIC 9(02)           VALUE ZEROS.
           03  WRK-MENSAGEM            PIC X(80)           VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*      AREA DO MODULO 5500     *'.
      *----------------------------------------------------------------*

       01  WRK-5500-AREA-SAIDA.
           03  WRK-5500-COD-RETORNO    PIC 9(02)           VALUE ZEROS.
           03  WRK-5500-COD-SQL-ERRO   PIC 9(03)           VALUE ZEROS.
           03  WRK-5500-DESC-MSG       PIC X(70)           VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*    AREA DO MODULO POOL1470   *'.
      *----------------------------------------------------------------*

       01  WRK-CAD-DATA.
           03  WRK-CAD-DIA             PIC 9(02)           VALUE ZEROS.
           03  WRK-CAD-MES             PIC 9(02)           VALUE ZEROS.
           03  WRK-CAD-ANO             PIC 9(04)           VALUE ZEROS.

       01  W-DATA.
           03  W-DIA                   PIC 9(02)          VALUE  ZEROS.
           03  FILLER                  PIC X(01)          VALUE SPACES.
           03  W-MES                   PIC 9(02)          VALUE  ZEROS.
           03  FILLER                  PIC X(01)          VALUE SPACES.
           03  W-ANO                   PIC 9(04)          VALUE  ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*    AREA DO MODULO GFCT5060   *'.
      *----------------------------------------------------------------*

       COPY 'I#GFCTBR'.
       COPY 'I#GFCTBS'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*    AREA DO MODULO GFCT9110   *'.
      *----------------------------------------------------------------*

       COPY 'I#GFCTIU'.
       COPY 'I#GFCTIV'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*    AREA DO MODULO MESU9015   *'.
      *----------------------------------------------------------------*

       COPY 'I#MESUM6'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*    AREA DO MODULO MESU9018   *'.
      *----------------------------------------------------------------*

       COPY 'I#MESUM8'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*      AREA DO MODULO 5000     *'.
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
           '*      AREA DO MODULO MESU     *'.
      *----------------------------------------------------------------*

       01  WRK-VERSAO-0315             PIC X(06)           VALUE SPACES.
       01  WRK-MENSAGEM-0315.
           03  WRK-COD-MSG-0315        PIC 9(03)           VALUE ZEROS.
           03  WRK-HIFEN-MENSAGEM-0315 PIC X(01)           VALUE SPACES.
           03  WRK-TEXTO-MENSAGEM-0315 PIC X(79)           VALUE SPACES.
       01  WRK-COD-FUNCIONAL-0315      PIC X(07)           VALUE SPACES.
       01  WRK-NOME-FUNC-0315          PIC X(40)           VALUE SPACES.
       01  WRK-COD-BANCO-0315          PIC 9(05)           VALUE ZEROS.
       01  WRK-NOME-BANCO-0315         PIC X(40)           VALUE SPACES.
       01  WRK-CNPJ-BANCO-0315.
ST25X6*    03  WRK-CNPJ-PRI-0315       PIC 9(09)           VALUE ZEROS.
ST25X6*    03  WRK-CNPJ-FIL-0315       PIC 9(05)           VALUE ZEROS.
ST25X6     03  WRK-CNPJ-PRI-0315       PIC X(09)           VALUE SPACES.
ST25X6     03  WRK-CNPJ-FIL-0315       PIC X(04)           VALUE SPACES.
           03  WRK-CNPJ-CON-0315       PIC 9(02)           VALUE ZEROS.
       01  WRK-COD-JUNCAO-0315         PIC 9(05)           VALUE ZEROS.
       01  WRK-NOME-DEPTO-0315         PIC X(40)           VALUE SPACES.
       01  WRK-SECAO-0315              PIC 9(03)           VALUE ZEROS.
       01  WRK-STATUS-0315             PIC X(01)           VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*      AREA DE TABELAS DB2     *'.
      *----------------------------------------------------------------*

           EXEC SQL
                INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB0A1
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB0A2
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB0A3
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB0A4
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB0A6
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB0A7
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB0A8
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB0A9
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB0B0
           END-EXEC.

           EXEC SQL
               DECLARE  CSR-GFCTB0A7 CURSOR FOR
               SELECT   CAGPTO_CTA,
                        CSERVC_TARIF,
                        DOCOR_EVNTO,
                        HIDTFD_ESTRN_AGPTO
               FROM     DB2PRD.PDIDO_ESTRN_GRP
               WHERE    CAGPTO_CTA        = :GFCTB0A7.CAGPTO-CTA
                 AND    CSERVC_TARIF      = :GFCTB0A7.CSERVC-TARIF
                 AND    DOCOR_EVNTO       = :GFCTB0A7.DOCOR-EVNTO
                 AND    CSIT_PROCM_ESTRN IN (2, 3)
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*    FIM DA WORKING GFCT0373   *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       COPY 'I#GFCTFR'.
       COPY 'I#GFCTFS'.
       COPY 'I#GFCT0M'.

      *================================================================*
       PROCEDURE                       DIVISION USING GFCTFR-ENTRADA
                                                      GFCTFS-SAIDA
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

           INITIALIZE                  SQLCA.

           PERFORM 11000-MONTAR-AREA-SAIDA.

           PERFORM 12000-VERIFICAR-ONLINE.

           PERFORM 13000-CONSISTIR-DADOS.

      *----------------------------------------------------------------*
       10000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       11000-MONTAR-AREA-SAIDA         SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTFS-SAIDA.

           INITIALIZE                  GFCTFS-SAIDA.

           MOVE 1010                   TO GFCTFS-LL.
           MOVE GFCTFR-TRANSACAO       TO GFCTFS-TRANSACAO.
           MOVE GFCTFR-FUNCAO          TO GFCTFS-FUNCAO.
           MOVE GFCTFR-TERMINAL        TO GFCTFS-TERMINAL.
           MOVE GFCTFR-FUNC-BDSCO      TO GFCTFS-FUNC-BDSCO.
           MOVE GFCTFR-FIXO            TO GFCTFS-FIXO.
           MOVE 'S'                    TO GFCTFS-FIM.
           MOVE ZEROS                  TO GFCTFS-ERRO
                                          GFCTFS-COD-SQL-ERRO
                                          GFCTFS-COD-MSG-ERRO
                                          WRK-5000-COD-MSG.

           PERFORM 11100-OBTER-DESC-MENSAGEM.

           MOVE WRK-5000-DESC-MSG      TO GFCTFS-DESC-MSG-ERRO.

      *----------------------------------------------------------------*
       11000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       11100-OBTER-DESC-MENSAGEM       SECTION.
      *----------------------------------------------------------------*

           MOVE 'O'                    TO WRK-5000-AMBIENTE.
           MOVE 'GFCT5000'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             WRK-5000-AREA-ENVIA
                                             WRK-5000-AREA-RECEBE.

           IF  WRK-5000-COD-RETORNO    NOT EQUAL ZEROS
               IF  WRK-5000-COD-RETORNO
                                       EQUAL 01
                   MOVE 1              TO GFCTFS-ERRO
                   MOVE ZEROS          TO GFCTFS-COD-SQL-ERRO
                   MOVE ZEROS          TO GFCTFS-COD-MSG-ERRO
                   MOVE WRK-5000-DESC-MSG
                                       TO GFCTFS-DESC-MSG-ERRO
               END-IF
               IF  WRK-5000-COD-RETORNO
                                       EQUAL 99
                   MOVE 1              TO GFCTFS-ERRO
                   MOVE WRK-5000-COD-SQL-ERRO
                                       TO GFCTFS-COD-SQL-ERRO
                   MOVE ZEROS          TO GFCTFS-COD-MSG-ERRO
                   MOVE WRK-5000-DESC-MSG
                                       TO GFCTFS-DESC-MSG-ERRO
               END-IF
               MOVE SPACES             TO GFCTFS-DADOS-RST
               PERFORM 30000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       11100-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       12000-VERIFICAR-ONLINE          SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                  WRK-5500-AREA-SAIDA.

           MOVE 'GFCT5500'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             WRK-5500-AREA-SAIDA.

           IF  WRK-5500-COD-RETORNO    NOT EQUAL ZEROS
               IF  WRK-5500-COD-RETORNO
                                       EQUAL 04
                   MOVE 1              TO GFCTFS-ERRO
                   MOVE ZEROS          TO GFCTFS-COD-SQL-ERRO
                   MOVE 0030           TO GFCTFS-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
                   PERFORM 11100-OBTER-DESC-MENSAGEM
                   MOVE WRK-5000-DESC-MSG
                                       TO GFCTFS-DESC-MSG-ERRO
                   PERFORM 30000-FINALIZAR
               END-IF
               IF  WRK-5500-COD-RETORNO
                                       EQUAL 99
                   MOVE SPACES         TO GFCT0M-AREA-ERROS
                   INITIALIZE          GFCT0M-AREA-ERROS
                   MOVE 'GFCT0373'     TO GFCT0M-PROGRAMA
                   MOVE 'DB2'          TO GFCT0M-TIPO-ACESSO
                   MOVE 'PARM_CTRL_GSTAO'
                                       TO GFCT0M-NOME-TAB
                   MOVE 'SELECT'       TO GFCT0M-COMANDO-SQL
                   MOVE WRK-5500-COD-SQL-ERRO
                                       TO GFCT0M-SQLCODE
                                          GFCTFS-COD-SQL-ERRO
                   MOVE WRK-5500-DESC-MSG
                                       TO GFCTFS-DESC-MSG-ERRO
                   MOVE SQLCA          TO GFCT0M-SQLCA-AREA
                   MOVE '0010'         TO GFCT0M-LOCAL
                   MOVE 9              TO GFCTFS-ERRO
                   MOVE 'GFCTB0A0'     TO WRK-NOME-TAB
                   MOVE SPACES         TO GFCTFS-DADOS-RST
                   PERFORM 30000-FINALIZAR
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       12000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       13000-CONSISTIR-DADOS           SECTION.
      *----------------------------------------------------------------*

           IF (GFCTFR-TRANSACAO        EQUAL SPACES) OR
              (GFCTFR-FUNCAO           EQUAL SPACES) OR
              (GFCTFR-FUNC-BDSCO       EQUAL SPACES) OR
              (GFCTFR-CAGPTO           NOT EQUAL 4 AND 14 AND 9 AND 15
                                                   AND 16 AND 17)
              MOVE 1                   TO GFCTFS-ERRO
                                          GFCTFS-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
              MOVE ZEROS               TO GFCTFS-COD-SQL-ERRO
              PERFORM 11100-OBTER-DESC-MENSAGEM
              MOVE WRK-5000-DESC-MSG   TO GFCTFS-DESC-MSG-ERRO
              PERFORM 30000-FINALIZAR
           END-IF.

           PERFORM 13100-SELECIONAR-GFCTB0A1.
           PERFORM 13200-VE-DATA.

      *----------------------------------------------------------------*
       13000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       13100-SELECIONAR-GFCTB0A1       SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT'                 TO CSIST-PRINC OF GFCTB0A1.

           EXEC SQL
               SELECT  DPROCM_ATUAL
               INTO    :GFCTB0A1.DPROCM-ATUAL
               FROM    DB2PRD.PARM_DATA_PROCM
               WHERE   CSIST_PRINC = :GFCTB0A1.CSIST-PRINC
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE SPACES             TO GFCT0M-AREA-ERROS
               INITIALIZE              GFCT0M-AREA-ERROS
               MOVE 'PARM_DATA_PROCM'  TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0020'             TO GFCT0M-LOCAL
               MOVE 0012               TO GFCTFS-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB0A1'         TO WRK-NOME-TAB

               PERFORM 13110-MOVER-ERRO-SQL

               MOVE SPACES             TO GFCTFS-DADOS-RST

               PERFORM 30000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       13100-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       13110-MOVER-ERRO-SQL            SECTION.
      *----------------------------------------------------------------*

           MOVE 9                      TO GFCTFS-ERRO.
           MOVE 'GFCT0373'             TO GFCT0M-PROGRAMA.
           MOVE 'DB2'                  TO GFCT0M-TIPO-ACESSO.
           MOVE SQLCA                  TO GFCT0M-SQLCA-AREA.
           MOVE SQLCODE                TO GFCT0M-SQLCODE
                                          GFCTFS-COD-SQL-ERRO
           MOVE SPACES                 TO GFCTFS-DESC-MSG-ERRO.

           PERFORM 11100-OBTER-DESC-MENSAGEM.

           STRING WRK-5000-DESC-MSG
                  WRK-MSG-TAB
           DELIMITED BY '  '           INTO GFCTFS-DESC-MSG-ERRO.

      *----------------------------------------------------------------*
       13110-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       13200-VE-DATA                   SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTFR-DATA-DEBITO TO W-DATA
           MOVE W-DIA              TO WRK-CAD-DIA
           MOVE W-MES              TO WRK-CAD-MES
           MOVE W-ANO              TO WRK-CAD-ANO
           CALL 'POOL1470'         USING WRK-CAD-DATA
           IF  RETURN-CODE         NOT EQUAL ZEROS
               MOVE 1                  TO GFCTFS-ERRO
               MOVE ZEROS              TO GFCTFS-COD-SQL-ERRO
               MOVE 0635               TO GFCTFS-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               PERFORM 11100-OBTER-DESC-MENSAGEM
               MOVE WRK-5000-DESC-MSG  TO GFCTFS-DESC-MSG-ERRO
               PERFORM 30000-FINALIZAR
           END-IF.

           MOVE GFCTFR-CSERVC-TARIF   TO CSERVC-TARIF      OF GFCTB0A2.

           INSPECT GFCTFR-DATA-DEBITO REPLACING ALL '/'    BY '.'.

           MOVE GFCTFR-DATA-DEBITO    TO DINIC-VGCIA-TARIF OF GFCTB0A2
                                         DFIM-VGCIA-TARIF  OF GFCTB0A2.

           EXEC SQL
             SELECT  CSERVC_TARIF
             INTO    :GFCTB0A2.CSERVC-TARIF
             FROM   DB2PRD.PARM_SERVC_TARIF
             WHERE  CSERVC_TARIF       =  :GFCTB0A2.CSERVC-TARIF
               AND  DINIC_VGCIA_TARIF  <= :GFCTB0A2.DINIC-VGCIA-TARIF
               AND  DFIM_VGCIA_TARIF   >= :GFCTB0A2.DFIM-VGCIA-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               MOVE SPACES             TO GFCT0M-AREA-ERROS
               INITIALIZE              GFCT0M-AREA-ERROS
               MOVE 'PARM_SERVC_TARIF' TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0030'             TO GFCT0M-LOCAL
               MOVE 0012               TO GFCTFS-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB0A2'         TO WRK-NOME-TAB

               PERFORM 13110-MOVER-ERRO-SQL

               MOVE SPACES             TO GFCTFS-DADOS-RST

               PERFORM 30000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 1                  TO GFCTFS-ERRO
               MOVE ZEROS              TO GFCTFS-COD-SQL-ERRO
               MOVE 0388               TO GFCTFS-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               PERFORM 11100-OBTER-DESC-MENSAGEM
               MOVE WRK-5000-DESC-MSG  TO GFCTFS-DESC-MSG-ERRO
               PERFORM 30000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       13200-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       20000-PROCESSAR                 SECTION.
      *----------------------------------------------------------------*

           EVALUATE GFCTFR-CAGPTO
               WHEN 15
                   PERFORM 21000-CONSISTIR-PAB
               WHEN 14
                   IF GFCTFR-ESTORNO-TOTAL
                                       NOT EQUAL 'S'
                      PERFORM 22000-CONSISTIR-AGENCIA
                   END-IF
               WHEN 16
                   PERFORM 23000-CONSISTIR-EMPRESA
               WHEN 17
                   PERFORM 24000-CONSISTIR-POSTAL
               WHEN 4
                   PERFORM 25000-CONSISTIR-CLIENTE
               WHEN OTHER
                   PERFORM 26000-CONSISTIR-SEGMENTO
           END-EVALUATE.

           PERFORM 27000-CONSISTIR-DATA.

           PERFORM 28000-VERIFICAR-DUPLICIDADE.

           PERFORM 29000-INSERIR.

      *----------------------------------------------------------------*
       20000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       21000-CONSISTIR-PAB             SECTION.
      *----------------------------------------------------------------*

           PERFORM 22000-CONSISTIR-AGENCIA.

           IF GFCTFR-ESTORNO-TOTAL     NOT EQUAL 'S'
              MOVE 'VRS001'            TO COMU-VERSAO
              MOVE 237                 TO COMU-COD-EMPR
              MOVE GFCTFR-CAGENCIA     TO COMU-COD-DEPDC
              MOVE GFCTFR-CPAB         TO COMU-COD-POSTO
              MOVE SPACES              TO COMU-CORR-BANCARIO
              MOVE 'MESU9018'          TO WRK-MODULO
              CALL 'POOL0081'          USING WRK-MODULO
                                             COMU-DADOS-9018
              IF RETURN-CODE           NOT EQUAL ZEROS
                 IF RETURN-CODE        EQUAL 4
                    MOVE 1             TO GFCTFS-ERRO
                    MOVE 0518          TO GFCTFS-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
                    MOVE ZEROS         TO GFCTFS-COD-SQL-ERRO
                    PERFORM 11100-OBTER-DESC-MENSAGEM
                    MOVE WRK-5000-DESC-MSG
                                       TO GFCTFS-DESC-MSG-ERRO
                    PERFORM 30000-FINALIZAR
                 ELSE
                    IF RETURN-CODE     EQUAL 8
                       MOVE 1          TO GFCTFS-ERRO
                       MOVE 0519       TO GFCTFS-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
                       MOVE ZEROS      TO GFCTFS-COD-SQL-ERRO
                       PERFORM 11100-OBTER-DESC-MENSAGEM
                       MOVE WRK-5000-DESC-MSG
                                       TO GFCTFS-DESC-MSG-ERRO
                       PERFORM 30000-FINALIZAR
                    ELSE
                       IF RETURN-CODE  EQUAL 12
                          MOVE 1       TO GFCTFS-ERRO
                          MOVE 0520    TO GFCTFS-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
                          MOVE ZEROS   TO GFCTFS-COD-SQL-ERRO
                          PERFORM 11100-OBTER-DESC-MENSAGEM
                          MOVE WRK-5000-DESC-MSG
                                       TO GFCTFS-DESC-MSG-ERRO
                          PERFORM 30000-FINALIZAR
                       END-IF
                    END-IF
                 END-IF
              ELSE
                 IF COMU-COD-TIPO-POSTO
                                       EQUAL 9
                    MOVE 1             TO GFCTFS-ERRO
                    MOVE 0594          TO GFCTFS-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
                    MOVE ZEROS         TO GFCTFS-COD-SQL-ERRO

                    PERFORM 11100-OBTER-DESC-MENSAGEM

                    MOVE WRK-5000-DESC-MSG
                                       TO GFCTFS-DESC-MSG-ERRO

                    PERFORM 30000-FINALIZAR
                 END-IF
              END-IF
           END-IF.

      *----------------------------------------------------------------*
       21000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       22000-CONSISTIR-AGENCIA         SECTION.
      *----------------------------------------------------------------*

           MOVE 237                    TO COMU-BANCO.
           MOVE GFCTFR-CAGENCIA        TO COMU-DEPDC.
           MOVE 'GFCT0373'             TO COMU-NOME-PGM.
           MOVE 'MESU9015'             TO WRK-MODULO.
           CALL 'POOL0081'             USING WRK-MODULO
                                             COMU-PARM
                                             WRK-POOL7100
                                             WRK-SQLCA.

           IF COMU-COD-RETORNO         NOT EQUAL ZEROS
              MOVE 1                   TO GFCTFS-ERRO
              MOVE 0465                TO GFCTFS-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
              MOVE ZEROS               TO GFCTFS-COD-SQL-ERRO
              PERFORM 11100-OBTER-DESC-MENSAGEM
              MOVE WRK-5000-DESC-MSG   TO GFCTFS-DESC-MSG-ERRO
              PERFORM 30000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       22000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       23000-CONSISTIR-EMPRESA         SECTION.
      *----------------------------------------------------------------*

           PERFORM 22000-CONSISTIR-AGENCIA.

           MOVE SPACES                 TO GFCTBR-ENTRADA.

           MOVE 100                    TO GFCTBR-LL.
           MOVE ZEROS                  TO GFCTBR-ZZ.

           MOVE GFCTFR-TRANSACAO       TO GFCTBR-TRANSACAO.
           MOVE GFCTFR-FUNCAO          TO GFCTBR-FUNCAO.
           MOVE ZEROS                  TO GFCTBR-QTDE-OCOR.
           MOVE ZEROS                  TO GFCTBR-QTDE-TOT-REG.
           MOVE GFCTFR-FUNC-BDSCO      TO GFCTBR-FUNC-BDSCO.
           MOVE 'N'                    TO GFCTBR-FIM.

           MOVE GFCTFR-CAGENCIA        TO GFCTBR-COD-DEPDC.
           MOVE GFCTFR-CCONTA          TO GFCTBR-CCTA-CLI.

           MOVE SPACES                 TO GFCTBS-SAIDA
                                          GFCT0M-AREA-ERROS.

           INITIALIZE                  GFCTBS-SAIDA
                                       GFCT0M-AREA-ERROS.

           MOVE 'GFCT5060'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTBR-ENTRADA
                                             GFCTBS-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 9                  TO GFCTFS-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-NOME-MODULO
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0373'         TO GFCT0M-TRANSACAO

               PERFORM 30000-FINALIZAR
           END-IF

           IF  GFCTBS-ERRO             NOT EQUAL ZEROS
               MOVE 'S'                TO GFCTFS-FIM
               MOVE GFCTBS-ERRO        TO GFCTFS-ERRO
               MOVE GFCTBS-COD-SQL-ERRO
                                       TO GFCTFS-COD-SQL-ERRO
               MOVE GFCTBS-COD-MSG-ERRO
                                       TO GFCTFS-COD-MSG-ERRO

               IF  GFCTBS-ERRO         EQUAL 1
                   MOVE GFCTBS-DESC-MSG-ERRO
                                       TO GFCTFS-DESC-MSG-ERRO
               ELSE
                   STRING 'GFCT5060-'
                          GFCTBS-DESC-MSG-ERRO
                   DELIMITED BY SIZE   INTO GFCTFS-DESC-MSG-ERRO
               END-IF

               PERFORM 30000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       23000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       24000-CONSISTIR-POSTAL          SECTION.
      *----------------------------------------------------------------*

           PERFORM 22000-CONSISTIR-AGENCIA.

           IF GFCTFR-ESTORNO-TOTAL     NOT EQUAL 'S'
              MOVE 'VRS001'            TO COMU-VERSAO
              MOVE 237                 TO COMU-COD-EMPR
              MOVE GFCTFR-CAGENCIA     TO COMU-COD-DEPDC
              MOVE GFCTFR-CPAB         TO COMU-COD-POSTO
              MOVE SPACES              TO COMU-CORR-BANCARIO
              MOVE 'MESU9018'          TO WRK-MODULO
              CALL 'POOL0081'          USING WRK-MODULO
                                             COMU-DADOS-9018
              IF RETURN-CODE           NOT EQUAL ZEROS
                 IF RETURN-CODE        EQUAL 4
                    MOVE 1             TO GFCTFS-ERRO
                    MOVE 0518          TO GFCTFS-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
                    MOVE ZEROS         TO GFCTFS-COD-SQL-ERRO
                    PERFORM 11100-OBTER-DESC-MENSAGEM
                    MOVE WRK-5000-DESC-MSG
                                       TO GFCTFS-DESC-MSG-ERRO
                    PERFORM 30000-FINALIZAR
                 ELSE
                    IF RETURN-CODE     EQUAL 8
                       MOVE 1          TO GFCTFS-ERRO
                       MOVE 0519       TO GFCTFS-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
                       MOVE ZEROS      TO GFCTFS-COD-SQL-ERRO
                       PERFORM 11100-OBTER-DESC-MENSAGEM
                       MOVE WRK-5000-DESC-MSG
                                       TO GFCTFS-DESC-MSG-ERRO
                       PERFORM 30000-FINALIZAR
                    ELSE
                       IF RETURN-CODE  EQUAL 12
                          MOVE 1       TO GFCTFS-ERRO
                          MOVE 0520    TO GFCTFS-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
                          MOVE ZEROS   TO GFCTFS-COD-SQL-ERRO
                          PERFORM 11100-OBTER-DESC-MENSAGEM
                          MOVE WRK-5000-DESC-MSG
                                       TO GFCTFS-DESC-MSG-ERRO
                          PERFORM 30000-FINALIZAR
                       END-IF
                    END-IF
                 END-IF
              ELSE
                 IF COMU-COD-TIPO-POSTO
                                       NOT EQUAL 9
                    MOVE 1             TO GFCTFS-ERRO
                    MOVE 0521          TO GFCTFS-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
                    MOVE ZEROS         TO GFCTFS-COD-SQL-ERRO
                    PERFORM 11100-OBTER-DESC-MENSAGEM
                    MOVE WRK-5000-DESC-MSG
                                       TO GFCTFS-DESC-MSG-ERRO
                    PERFORM 30000-FINALIZAR
                 END-IF
              END-IF
           END-IF.

      *----------------------------------------------------------------*
       24000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       25000-CONSISTIR-CLIENTE         SECTION.
      *----------------------------------------------------------------*

ST2506*    M0VE SPACES                 TO WRK-CONTROLE.
ST2506*    M0VE GFCTFR-CGC-CPF         TO WRK-CPF.
ST2506*    M0VE GFCTFR-FILIAL          TO WRK-FILIAL.
ST2506*
ST2506*    CA1L 'POOL0110'             USING WRK-CPF-CGC
ST2506*                                      WRK-CONTROLE.

ST2506     MOVE ZEROS                  TO WRK-CAD-CONTROLE. 
ST2506     MOVE GFCTFR-CGC-CPF         TO WRK-CAD-NUMERO.
ST2506     MOVE GFCTFR-FILIAL          TO WRK-CAD-FILIAL.
ST2506
ST2506     CALL WRK-BRAD2000     USING WRK-CAD-CPFCGC
ST2506                                 WRK-CAD-CONTROLE.

           IF RETURN-CODE              NOT EQUAL ZEROS
              MOVE 1                   TO GFCTFS-ERRO
              MOVE 0468                TO GFCTFS-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
              MOVE ZEROS               TO GFCTFS-COD-SQL-ERRO
              PERFORM 11100-OBTER-DESC-MENSAGEM
              MOVE WRK-5000-DESC-MSG   TO GFCTFS-DESC-MSG-ERRO
              PERFORM 30000-FINALIZAR
           END-IF.

ST2506     MOVE WRK-CAD-CONTROLE       TO WRK-9-02.
ST2506     IF  GFCTFR-CONTROLE         NOT EQUAL WRK-9-X-2 
ST2506*    1F GFCTFR-CONTROLE          NOT EQUAL WRK-CONTROLE
              MOVE 1                   TO GFCTFS-ERRO
              MOVE 0507                TO GFCTFS-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
              MOVE ZEROS               TO GFCTFS-COD-SQL-ERRO
              PERFORM 11100-OBTER-DESC-MENSAGEM
              MOVE WRK-5000-DESC-MSG   TO GFCTFS-DESC-MSG-ERRO
              PERFORM 30000-FINALIZAR
           END-IF.

           PERFORM 25100-PESQUISAR-BASE-CLIE.

      *----------------------------------------------------------------*
       25000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       25100-PESQUISAR-BASE-CLIE       SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO GFCTIU-ENTRADA.

           INITIALIZE                  GFCTIU-ENTRADA.

           MOVE 100                    TO GFCTIU-LL.
           MOVE ZEROS                  TO GFCTIU-ZZ.

           MOVE GFCTFR-TRANSACAO       TO GFCTIU-TRANSACAO.
           MOVE GFCTFR-FUNCAO          TO GFCTIU-FUNCAO.
           MOVE GFCTFR-FUNC-BDSCO      TO GFCTIU-FUNC-BDSCO.
           MOVE 'N'                    TO GFCTIU-FIM.
ST2506*    MOVE GFCTFR-CGC-CPF         TO GFCTIU-NRO-CPF-CNPJ
ST2506*    MOVE GFCTFR-FILIAL          TO GFCTIU-NRO-FILIAL
ST2506     MOVE GFCTFR-CGC-CPF         TO GFCTIU-COD-CPF-CNPJ
ST2506     MOVE GFCTFR-FILIAL          TO GFCTIU-COD-FILIAL
           MOVE GFCTFR-CONTROLE        TO GFCTIU-CTRL-CPF-CNPJ

           MOVE SPACES                 TO GFCTIV-SAIDA
                                          GFCT0M-AREA-ERROS.

           INITIALIZE                  GFCTIV-SAIDA
                                       GFCT0M-AREA-ERROS.

           MOVE 'GFCT5110'             TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTIU-ENTRADA
                                             GFCTIV-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 9                  TO GFCTFS-ERRO
               MOVE WRK-MODULO         TO GFCT0M-PROGRAMA
                                          WRK-NOME-MODULO
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               MOVE 'APL'              TO GFCT0M-TIPO-ACESSO
               MOVE 'GFCT0373'         TO GFCT0M-TRANSACAO

               PERFORM 30000-FINALIZAR
           END-IF

           IF  GFCTIV-ERRO             NOT EQUAL ZEROS
               MOVE 'S'                TO GFCTFS-FIM
               MOVE GFCTIV-ERRO        TO GFCTFS-ERRO
               MOVE GFCTIV-COD-SQL-ERRO
                                       TO GFCTFS-COD-SQL-ERRO
               MOVE GFCTIV-COD-MSG-ERRO
                                       TO GFCTFS-COD-MSG-ERRO

               IF  GFCTIV-ERRO         EQUAL 1
                   MOVE GFCTIV-DESC-MSG-ERRO
                                       TO GFCTFS-DESC-MSG-ERRO
               ELSE
                   STRING 'GFCT5110-'
                      GFCTIV-DESC-MSG-ERRO
                   DELIMITED BY SIZE   INTO GFCTFS-DESC-MSG-ERRO
               END-IF

               PERFORM 30000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       25100-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       26000-CONSISTIR-SEGMENTO        SECTION.
      *----------------------------------------------------------------*

      *    NAO HA NECESSIDADE DE CONSITENCIA
      *    O CODIGO DO SEGMENTO EH O ESCOLHIDO NA COMBO

      *----------------------------------------------------------------*
       26000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       27000-CONSISTIR-DATA            SECTION.
      *----------------------------------------------------------------*

           PERFORM 27100-CHAMAR-POOL1570.

           STRING GFCTFR-DATA-DEBITO(07:04)
                  GFCTFR-DATA-DEBITO(04:02)
                  GFCTFR-DATA-DEBITO(01:02)
           DELIMITED BY SIZE           INTO WRK-DATA-DEBITO.

           STRING DPROCM-ATUAL OF GFCTB0A1(07:04)
                  DPROCM-ATUAL OF GFCTB0A1(04:02)
                  DPROCM-ATUAL OF GFCTB0A1(01:02)
           DELIMITED BY SIZE           INTO WRK-DATA-PROCM.

           IF  WRK-DATA-DEBITO         NOT LESS WRK-DATA-PROCM
               MOVE 1                  TO GFCTFS-ERRO
               MOVE ZEROS              TO GFCTFS-COD-SQL-ERRO
               MOVE 0462               TO GFCTFS-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               PERFORM 11100-OBTER-DESC-MENSAGEM
               MOVE WRK-5000-DESC-MSG  TO GFCTFS-DESC-MSG-ERRO
               PERFORM 30000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       27000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       27100-CHAMAR-POOL1570           SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO WRK-AREA-1570.

           INITIALIZE                  WRK-AREA-1570.

           INSPECT GFCTFR-DATA-DEBITO  REPLACING ALL '/' BY '.'.

           MOVE GFCTFR-DATA-DEBITO     TO WRK-DATA.
           MOVE 9                      TO WRK-FORMATO-DATA.

           CALL 'POOL1570'             USING WRK-DATA
                                             WRK-FORMATO-DATA
                                             WRK-MENSAGEM.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 1                  TO GFCTFS-ERRO
               MOVE ZEROS              TO GFCTFS-COD-SQL-ERRO
               MOVE 0460               TO GFCTFS-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               PERFORM 11100-OBTER-DESC-MENSAGEM
               MOVE WRK-5000-DESC-MSG  TO GFCTFS-DESC-MSG-ERRO
               PERFORM 30000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       27100-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       28000-VERIFICAR-DUPLICIDADE     SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTFR-CAGPTO          TO CAGPTO-CTA   OF GFCTB0A7.
           MOVE GFCTFR-CSERVC-TARIF    TO CSERVC-TARIF OF GFCTB0A7.
           MOVE GFCTFR-DATA-DEBITO     TO DOCOR-EVNTO  OF GFCTB0A7.

           MOVE 'N'                    TO WRK-FIM-CURSOR-GFCTB0A7.

           PERFORM 28100-ABRIR-CURSOR.

           PERFORM 28200-LER-CURSOR.

           MOVE 'N'                    TO WRK-ACHOU-DUPLICIDADE.

           PERFORM 28300-PROCESSAR-CURSOR
             UNTIL WRK-FIM-CURSOR-GFCTB0A7
                                       EQUAL 'S'
                OR WRK-ACHOU-DUPLICIDADE
                                       EQUAL 'S'.

           PERFORM 28400-FECHAR-CURSOR.

           IF  WRK-ACHOU-DUPLICIDADE   EQUAL 'S'
               MOVE 1                  TO GFCTFS-ERRO
               MOVE ZEROS              TO GFCTFS-COD-SQL-ERRO
               MOVE 0467               TO GFCTFS-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               PERFORM 11100-OBTER-DESC-MENSAGEM
               MOVE WRK-5000-DESC-MSG  TO GFCTFS-DESC-MSG-ERRO
               PERFORM 30000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       28000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       28100-ABRIR-CURSOR              SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               OPEN CSR-GFCTB0A7
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE 'GFCTB0A7'         TO WRK-NOME-TAB
               MOVE 'PDIDO_ESTRN_GRP'  TO GFCT0M-NOME-TAB
               MOVE 'OPEN'             TO GFCT0M-COMANDO-SQL
               MOVE '0040'             TO GFCT0M-LOCAL
               MOVE 0005               TO GFCTFS-COD-MSG-ERRO
                                          WRK-5000-COD-MSG

               PERFORM 13110-MOVER-ERRO-SQL

               MOVE SPACES             TO GFCTFS-DADOS-RST

               PERFORM 30000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       28100-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       28200-LER-CURSOR                SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               FETCH CSR-GFCTB0A7
               INTO :GFCTB0A7.CAGPTO-CTA,
                    :GFCTB0A7.CSERVC-TARIF,
                    :GFCTB0A7.DOCOR-EVNTO,
                    :GFCTB0A7.HIDTFD-ESTRN-AGPTO
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE 'GFCTB0A7'         TO WRK-NOME-TAB
               MOVE 'PDIDO_ESTRN_GRP'  TO GFCT0M-NOME-TAB
               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE '0050'             TO GFCT0M-LOCAL
               MOVE 0006               TO GFCTFS-COD-MSG-ERRO
                                          WRK-5000-COD-MSG

               PERFORM 13110-MOVER-ERRO-SQL

               MOVE SPACES             TO GFCTFS-DADOS-RST

               PERFORM 30000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FIM-CURSOR-GFCTB0A7
           END-IF.

      *----------------------------------------------------------------*
       28200-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       28300-PROCESSAR-CURSOR          SECTION.
      *----------------------------------------------------------------*


           EVALUATE GFCTFR-CAGPTO
               WHEN 14
                   PERFORM 28310-VERIFICAR-AGENCIA
               WHEN 4
                   PERFORM 28320-VERIFICAR-CLIENTE
               WHEN 16
                   PERFORM 28330-VERIFICAR-EMPRESA
               WHEN 15
                   PERFORM 28340-VERIFICAR-POSTO
               WHEN 17
                   PERFORM 28350-VERIFICAR-POSTAL
               WHEN OTHER
                   PERFORM 28360-VERIFICAR-SEGMENTO
           END-EVALUATE.

           PERFORM 28200-LER-CURSOR.

      *----------------------------------------------------------------*
       28300-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       28310-VERIFICAR-AGENCIA         SECTION.
      *----------------------------------------------------------------*

           MOVE CAGPTO-CTA OF GFCTB0A7 TO CAGPTO-CTA        OF GFCTB0A3.
           MOVE CSERVC-TARIF OF GFCTB0A7
                                       TO CSERVC-TARIF      OF GFCTB0A3.
           MOVE DOCOR-EVNTO OF GFCTB0A7
                                       TO DOCOR-EVNTO       OF GFCTB0A3.
           MOVE HIDTFD-ESTRN-AGPTO OF GFCTB0A7
                                      TO HIDTFD-ESTRN-AGPTO OF GFCTB0A3.
           MOVE 237                    TO CEMPR-INC         OF GFCTB0A3.

           IF  GFCTFR-ESTORNO-TOTAL    EQUAL 'S'
               MOVE ZEROS              TO CDEPDC            OF GFCTB0A3
           ELSE
               MOVE GFCTFR-CAGENCIA    TO CDEPDC            OF GFCTB0A3
           END-IF.

           EXEC SQL
               SELECT  CEMPR_INC,
                       CDEPDC
               INTO   :GFCTB0A3.CEMPR-INC,
                      :GFCTB0A3.CDEPDC
               FROM    DB2PRD.PDIDO_ESTRN_AG
               WHERE   CAGPTO_CTA         = :GFCTB0A3.CAGPTO-CTA
                 AND   CSERVC_TARIF       = :GFCTB0A3.CSERVC-TARIF
                 AND   DOCOR_EVNTO        = :GFCTB0A3.DOCOR-EVNTO
                 AND   HIDTFD_ESTRN_AGPTO = :GFCTB0A3.HIDTFD-ESTRN-AGPTO
                 AND   CEMPR_INC          = :GFCTB0A3.CEMPR-INC
                 AND   CDEPDC             = :GFCTB0A3.CDEPDC
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-AREA-ERROS
               MOVE 'GFCTB0A3'         TO WRK-NOME-TAB
               MOVE 'PDIDO_ESTRN_AG'   TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0060'             TO GFCT0M-LOCAL
               MOVE 0012               TO GFCTFS-COD-MSG-ERRO
                                          WRK-5000-COD-MSG

               PERFORM 13110-MOVER-ERRO-SQL

               MOVE SPACES             TO GFCTFS-DADOS-RST

               PERFORM 30000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS
               MOVE 'S'                TO WRK-ACHOU-DUPLICIDADE
           END-IF.

      *----------------------------------------------------------------*
       28310-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       28320-VERIFICAR-CLIENTE         SECTION.
      *----------------------------------------------------------------*

           MOVE CAGPTO-CTA OF GFCTB0A7 TO CAGPTO-CTA        OF GFCTB0A4.
           MOVE CSERVC-TARIF OF GFCTB0A7
                                       TO CSERVC-TARIF      OF GFCTB0A4.
           MOVE DOCOR-EVNTO OF GFCTB0A7
                                       TO DOCOR-EVNTO       OF GFCTB0A4.
           MOVE HIDTFD-ESTRN-AGPTO OF GFCTB0A7
                                      TO HIDTFD-ESTRN-AGPTO OF GFCTB0A4.
ST2506*    MOVE GFCTFR-CGC-CPF         TO CCGC-CPF          OF GFCTB0A4.
ST2506*    MOVE GFCTFR-FILIAL          TO CFLIAL-CGC        OF GFCTB0A4.
ST2506*    MOVE GFCTFR-CONTROLE        TO CCTRL-CPF-CGC     OF GFCTB0A4.
ST2506     MOVE GFCTFR-CGC-CPF         TO CCGC-CPF-ST       OF GFCTB0A4.
ST2506     MOVE GFCTFR-FILIAL          TO CFLIAL-CGC-ST     OF GFCTB0A4.
ST2506     MOVE GFCTFR-CONTROLE        TO CCTRL-CPF-CGC-ST  OF GFCTB0A4.

           EXEC SQL
ST2506*        SELECT  CCGC_CPF,
ST2506*                CFLIAL_CGC,
ST2506*                CCTRL_CPF_CGC
ST2506*        INTO   :GFCTB0A4.CCGC-CPF,
ST2506*               :GFCTB0A4.CFLIAL-CGC,
ST2506*               :GFCTB0A4.CCTRL-CPF-CGC
ST2506         SELECT  CCGC_CPF_ST,
ST2506                 CFLIAL_CGC_ST,
ST2506                 CCTRL_CPF_CGC_ST
ST2506         INTO   :GFCTB0A4.CCGC-CPF-ST,
ST2506                :GFCTB0A4.CFLIAL-CGC-ST,
ST2506                :GFCTB0A4.CCTRL-CPF-CGC-ST
               FROM    DB2PRD.PDIDO_ESTRN_CLI
               WHERE   CAGPTO_CTA         = :GFCTB0A4.CAGPTO-CTA
                 AND   CSERVC_TARIF       = :GFCTB0A4.CSERVC-TARIF
                 AND   DOCOR_EVNTO        = :GFCTB0A4.DOCOR-EVNTO
                 AND   HIDTFD_ESTRN_AGPTO = :GFCTB0A4.HIDTFD-ESTRN-AGPTO
ST2506*          AND   CCGC_CPF           = :GFCTB0A4.CCGC-CPF
ST2506*          AND   CFLIAL_CGC         = :GFCTB0A4.CFLIAL-CGC
ST2506*          AND   CCTRL_CPF_CGC      = :GFCTB0A4.CCTRL-CPF-CGC
ST2506           AND   CCGC_CPF_ST        = :GFCTB0A4.CCGC-CPF-ST
ST2506           AND   CFLIAL_CGC_ST      = :GFCTB0A4.CFLIAL-CGC-ST
ST2506           AND   CCTRL_CPF_CGC_ST   = :GFCTB0A4.CCTRL-CPF-CGC-ST
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-AREA-ERROS
               MOVE 'GFCTB0A4'         TO WRK-NOME-TAB
               MOVE 'PDIDO_ESTRN_CLI'  TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0070'             TO GFCT0M-LOCAL
               MOVE 0012               TO GFCTFS-COD-MSG-ERRO
                                          WRK-5000-COD-MSG

               PERFORM 13110-MOVER-ERRO-SQL

               MOVE SPACES             TO GFCTFS-DADOS-RST

               PERFORM 30000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS
               MOVE 'S'                TO WRK-ACHOU-DUPLICIDADE
           END-IF.

      *----------------------------------------------------------------*
       28320-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       28330-VERIFICAR-EMPRESA         SECTION.
      *----------------------------------------------------------------*

           MOVE CAGPTO-CTA OF GFCTB0A7 TO CAGPTO-CTA        OF GFCTB0A6.
           MOVE CSERVC-TARIF OF GFCTB0A7
                                       TO CSERVC-TARIF      OF GFCTB0A6.
           MOVE DOCOR-EVNTO OF GFCTB0A7
                                       TO DOCOR-EVNTO       OF GFCTB0A6.
           MOVE HIDTFD-ESTRN-AGPTO OF GFCTB0A7
                                      TO HIDTFD-ESTRN-AGPTO OF GFCTB0A6.
           MOVE GFCTFR-CAGENCIA        TO CAG-EMPR          OF GFCTB0A6.
           MOVE GFCTFR-CCONTA          TO CNRO-CTA-DEB      OF GFCTB0A6.

           EXEC SQL
               SELECT  CAG_EMPR,
                       CNRO_CTA_DEB
               INTO   :GFCTB0A6.CAG-EMPR,
                      :GFCTB0A6.CNRO-CTA-DEB
               FROM    DB2PRD.PDIDO_ESTRN_EMPR
               WHERE   CAGPTO_CTA         = :GFCTB0A6.CAGPTO-CTA
                 AND   CSERVC_TARIF       = :GFCTB0A6.CSERVC-TARIF
                 AND   DOCOR_EVNTO        = :GFCTB0A6.DOCOR-EVNTO
                 AND   HIDTFD_ESTRN_AGPTO = :GFCTB0A6.HIDTFD-ESTRN-AGPTO
                 AND   CAG_EMPR           = :GFCTB0A6.CAG-EMPR
                 AND   CNRO_CTA_DEB       = :GFCTB0A6.CNRO-CTA-DEB
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-AREA-ERROS
               MOVE 'GFCTB0A6'         TO WRK-NOME-TAB
               MOVE 'PDIDO_ESTRN_EMPR' TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0080'             TO GFCT0M-LOCAL
               MOVE 0012               TO GFCTFS-COD-MSG-ERRO
                                          WRK-5000-COD-MSG

               PERFORM 13110-MOVER-ERRO-SQL

               MOVE SPACES             TO GFCTFS-DADOS-RST

               PERFORM 30000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS
               MOVE 'S'                TO WRK-ACHOU-DUPLICIDADE
           END-IF.

      *----------------------------------------------------------------*
       28330-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       28340-VERIFICAR-POSTO           SECTION.
      *----------------------------------------------------------------*

           MOVE CAGPTO-CTA OF GFCTB0A7 TO CAGPTO-CTA        OF GFCTB0A8.
           MOVE CSERVC-TARIF OF GFCTB0A7
                                       TO CSERVC-TARIF      OF GFCTB0A8.
           MOVE DOCOR-EVNTO OF GFCTB0A7
                                       TO DOCOR-EVNTO       OF GFCTB0A8.
           MOVE HIDTFD-ESTRN-AGPTO OF GFCTB0A7
                                      TO HIDTFD-ESTRN-AGPTO OF GFCTB0A8.
           MOVE 237                    TO CEMPR-INC         OF GFCTB0A8.
           MOVE GFCTFR-CAGENCIA        TO CDEPDC            OF GFCTB0A8.

           IF  GFCTFR-ESTORNO-TOTAL    EQUAL 'S'
               MOVE ZEROS              TO CPOSTO-SERVC      OF GFCTB0A8
           ELSE
               MOVE GFCTFR-CPAB        TO CPOSTO-SERVC      OF GFCTB0A8
           END-IF.

           EXEC SQL
               SELECT  CEMPR_INC,
                       CDEPDC,
                       CPOSTO_SERVC
               INTO   :GFCTB0A8.CEMPR-INC,
                      :GFCTB0A8.CDEPDC,
                      :GFCTB0A8.CPOSTO-SERVC
               FROM    DB2PRD.PDIDO_ESTRN_PAB
               WHERE   CAGPTO_CTA         = :GFCTB0A8.CAGPTO-CTA
                 AND   CSERVC_TARIF       = :GFCTB0A8.CSERVC-TARIF
                 AND   DOCOR_EVNTO        = :GFCTB0A8.DOCOR-EVNTO
                 AND   HIDTFD_ESTRN_AGPTO = :GFCTB0A8.HIDTFD-ESTRN-AGPTO
                 AND   CEMPR_INC          = :GFCTB0A8.CEMPR-INC
                 AND   CDEPDC             = :GFCTB0A8.CDEPDC
                 AND   CPOSTO_SERVC       = :GFCTB0A8.CPOSTO-SERVC
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-AREA-ERROS
               MOVE 'GFCTB0A8'         TO WRK-NOME-TAB
               MOVE 'PDIDO_ESTRN_PAB'  TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0090'             TO GFCT0M-LOCAL
               MOVE 0012               TO GFCTFS-COD-MSG-ERRO
                                          WRK-5000-COD-MSG

               PERFORM 13110-MOVER-ERRO-SQL

               MOVE SPACES             TO GFCTFS-DADOS-RST

               PERFORM 30000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS
               MOVE 'S'                TO WRK-ACHOU-DUPLICIDADE
           END-IF.

      *----------------------------------------------------------------*
       28340-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       28350-VERIFICAR-POSTAL          SECTION.
      *----------------------------------------------------------------*

           MOVE CAGPTO-CTA OF GFCTB0A7 TO CAGPTO-CTA        OF GFCTB0A9.
           MOVE CSERVC-TARIF OF GFCTB0A7
                                       TO CSERVC-TARIF      OF GFCTB0A9.
           MOVE DOCOR-EVNTO OF GFCTB0A7
                                       TO DOCOR-EVNTO       OF GFCTB0A9.
           MOVE HIDTFD-ESTRN-AGPTO OF GFCTB0A7
                                      TO HIDTFD-ESTRN-AGPTO OF GFCTB0A9.
           MOVE 237                    TO CEMPR-INC         OF GFCTB0A9.
           MOVE GFCTFR-CAGENCIA        TO CDEPDC            OF GFCTB0A9.

           IF  GFCTFR-ESTORNO-TOTAL    EQUAL 'S'
               MOVE ZEROS              TO CPOSTO-SERVC      OF GFCTB0A9
           ELSE
               MOVE GFCTFR-CPAB        TO CPOSTO-SERVC      OF GFCTB0A9
           END-IF.

           EXEC SQL
               SELECT  CEMPR_INC,
                       CDEPDC,
                       CPOSTO_SERVC
               INTO   :GFCTB0A9.CEMPR-INC,
                      :GFCTB0A9.CDEPDC,
                      :GFCTB0A9.CPOSTO-SERVC
               FROM    DB2PRD.PDIDO_ESTRN_PSTAL
               WHERE   CAGPTO_CTA         = :GFCTB0A9.CAGPTO-CTA
                 AND   CSERVC_TARIF       = :GFCTB0A9.CSERVC-TARIF
                 AND   DOCOR_EVNTO        = :GFCTB0A9.DOCOR-EVNTO
                 AND   HIDTFD_ESTRN_AGPTO = :GFCTB0A9.HIDTFD-ESTRN-AGPTO
                 AND   CEMPR_INC          = :GFCTB0A9.CEMPR-INC
                 AND   CDEPDC             = :GFCTB0A9.CDEPDC
                 AND   CPOSTO_SERVC       = :GFCTB0A9.CPOSTO-SERVC
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-AREA-ERROS
               MOVE 'GFCTB0A9'         TO WRK-NOME-TAB
               MOVE 'PDIDO_ESTRN_PSTAL'
                                       TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0100'             TO GFCT0M-LOCAL
               MOVE 0012               TO GFCTFS-COD-MSG-ERRO
                                          WRK-5000-COD-MSG

               PERFORM 13110-MOVER-ERRO-SQL

               MOVE SPACES             TO GFCTFS-DADOS-RST

               PERFORM 30000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS
               MOVE 'S'                TO WRK-ACHOU-DUPLICIDADE
           END-IF.

      *----------------------------------------------------------------*
       28350-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       28360-VERIFICAR-SEGMENTO        SECTION.
      *----------------------------------------------------------------*

           MOVE CAGPTO-CTA OF GFCTB0A7 TO CAGPTO-CTA        OF GFCTB0B0.
           MOVE CSERVC-TARIF OF GFCTB0A7
                                       TO CSERVC-TARIF      OF GFCTB0B0.
           MOVE DOCOR-EVNTO OF GFCTB0A7
                                       TO DOCOR-EVNTO       OF GFCTB0B0.
           MOVE HIDTFD-ESTRN-AGPTO OF GFCTB0A7
                                      TO HIDTFD-ESTRN-AGPTO OF GFCTB0B0.
           MOVE GFCTFR-CSEGMENTO       TO CSGMTO-GSTAO-TARIF
                                                            OF GFCTB0B0.

           EXEC SQL
               SELECT  CSGMTO_GSTAO_TARIF
               INTO   :GFCTB0B0.CSGMTO-GSTAO-TARIF
               FROM    DB2PRD.PDIDO_ESTRN_SGMTO
               WHERE   CAGPTO_CTA         = :GFCTB0B0.CAGPTO-CTA
                 AND   CSERVC_TARIF       = :GFCTB0B0.CSERVC-TARIF
                 AND   DOCOR_EVNTO        = :GFCTB0B0.DOCOR-EVNTO
                 AND   HIDTFD_ESTRN_AGPTO = :GFCTB0B0.HIDTFD-ESTRN-AGPTO
                 AND   CSGMTO_GSTAO_TARIF = :GFCTB0B0.CSGMTO-GSTAO-TARIF
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-AREA-ERROS
               MOVE 'GFCTB0B0'         TO WRK-NOME-TAB
               MOVE 'PDIDO_ESTRN_SGMTO'
                                       TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE '0110'             TO GFCT0M-LOCAL
               MOVE 0012               TO GFCTFS-COD-MSG-ERRO
                                          WRK-5000-COD-MSG

               PERFORM 13110-MOVER-ERRO-SQL

               MOVE SPACES             TO GFCTFS-DADOS-RST

               PERFORM 30000-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS
               MOVE 'S'                TO WRK-ACHOU-DUPLICIDADE
           END-IF.

      *----------------------------------------------------------------*
       28360-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       28400-FECHAR-CURSOR             SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               CLOSE CSR-GFCTB0A7
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE 'GFCTB0A7'         TO WRK-NOME-TAB
               MOVE 'PDIDO_ESTRN_GRP'  TO GFCT0M-NOME-TAB
               MOVE 'CLOSE'            TO GFCT0M-COMANDO-SQL
               MOVE '0120'             TO GFCT0M-LOCAL
               MOVE 0011               TO GFCTFS-COD-MSG-ERRO
                                          WRK-5000-COD-MSG

               PERFORM 13110-MOVER-ERRO-SQL

               MOVE SPACES             TO GFCTFS-DADOS-RST

               PERFORM 30000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       28400-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       29000-INSERIR                   SECTION.
      *----------------------------------------------------------------*

           PERFORM 29100-MOVER-DADOS-A7.

           PERFORM 29200-INSERIR-A7.

           EVALUATE GFCTFR-CAGPTO
               WHEN 15
                   PERFORM 29300-GERAR-PAB
               WHEN 14
                   PERFORM 29400-GERAR-AGENCIA
               WHEN 16
                   PERFORM 29500-GERAR-EMPRESA
               WHEN 17
                   PERFORM 29600-GERAR-POSTAL
               WHEN 4
                   PERFORM 29700-GERAR-CLIENTE
               WHEN OTHER
                   PERFORM 29800-GERAR-SEGMENTO
           END-EVALUATE.

      *----------------------------------------------------------------*
       29000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       29100-MOVER-DADOS-A7            SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTFR-CAGPTO          TO CAGPTO-CTA        OF GFCTB0A7.
           MOVE GFCTFR-CSERVC-TARIF    TO CSERVC-TARIF      OF GFCTB0A7.
           MOVE GFCTFR-DATA-DEBITO     TO DOCOR-EVNTO       OF GFCTB0A7.

           EXEC SQL
                SET :WRK-HORA          = CURRENT TIMESTAMP
           END-EXEC.

           MOVE WRK-HORA               TO HIDTFD-ESTRN-AGPTO
                                                            OF GFCTB0A7
                                          HSOLTC-ESTRN      OF GFCTB0A7.
           MOVE GFCTFR-FUNC-BDSCO      TO WRK-FUNC-BDSCO.
           MOVE WRK-FUNC-BDSCO         TO CFUNC-SOLTC-ESTRN OF GFCTB0A7.
           MOVE ZEROS                  TO CFUNC-PAREC-ESTRN OF GFCTB0A7.
           MOVE GFCTFR-COPERACAO       TO CTPO-OPER-MOTVO   OF GFCTB0A7.
           MOVE GFCTFR-CMOTIVO         TO CMOTVO-JUSTF      OF GFCTB0A7.
           MOVE ZEROS                  TO CDEPDC-PAREC-ESTRN
                                                            OF GFCTB0A7.
           MOVE '0001-01-01-01.01.01.000001'
                                       TO HPAREC-ESTRN      OF GFCTB0A7.
           MOVE GFCTFR-COMENTARIO      TO RJUSTF-SOLTC-ESTRN-TEXT
                                                            OF GFCTB0A7.
           MOVE LENGTH OF RJUSTF-SOLTC-ESTRN-TEXT OF GFCTB0A7
                                       TO RJUSTF-SOLTC-ESTRN-LEN
                                                            OF GFCTB0A7.
           MOVE ZEROS                  TO QESTRN-AGPTO      OF GFCTB0A7
           MOVE 2                      TO CSIT-PROCM-ESTRN  OF GFCTB0A7
           MOVE ZEROS                  TO VESTRN-SOLTD      OF GFCTB0A7.
           MOVE 1                      TO CSIT-SOLTC-ESTRN  OF GFCTB0A7.
           MOVE SPACES                 TO RJUSTF-PAREC-ESTRN-TEXT
                                                            OF GFCTB0A7.
           MOVE LENGTH OF RJUSTF-PAREC-ESTRN-TEXT OF GFCTB0A7
                                       TO RJUSTF-PAREC-ESTRN-LEN
                                                            OF GFCTB0A7.
           MOVE GFCTFR-ESTORNO-TOTAL   TO CINDCD-AGPTO-TOT  OF GFCTB0A7.
           MOVE ZEROS                  TO VCANCT-SOLTD      OF GFCTB0A7
                                          QCANCT-AGPTO      OF GFCTB0A7.
           MOVE '01.01.0001'           TO DEFETV-CREDT-ESTRN
                                                            OF GFCTB0A7.

           PERFORM 29110-OBTER-DEPENDENCIA.

           MOVE WRK-COD-JUNCAO-0315    TO CDEPDC-SOLTC-ESTRN
                                                            OF GFCTB0A7.

      *----------------------------------------------------------------*
       29100-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       29110-OBTER-DEPENDENCIA         SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO WRK-AREA-1570.

           INITIALIZE                  WRK-AREA-1570.

           MOVE GFCTFR-FUNC-BDSCO      TO WRK-COD-FUNCIONAL-0315.
           MOVE 'VRS001'               TO WRK-VERSAO-0315.

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
               MOVE 1                  TO GFCTFS-ERRO
               MOVE ZEROS              TO GFCTFS-COD-SQL-ERRO
               MOVE 0397               TO GFCTFS-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               PERFORM 11100-OBTER-DESC-MENSAGEM
               MOVE WRK-5000-DESC-MSG  TO GFCTFS-DESC-MSG-ERRO
               PERFORM 30000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       29110-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       29200-INSERIR-A7                SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             INSERT INTO DB2PRD.PDIDO_ESTRN_GRP
                  (CAGPTO_CTA,
                   CSERVC_TARIF,
                   DOCOR_EVNTO,
                   HIDTFD_ESTRN_AGPTO,
                   CSIT_SOLTC_ESTRN,
                   CSIT_PROCM_ESTRN,
                   CINDCD_AGPTO_TOT,
                   CDEPDC_SOLTC_ESTRN,
                   CFUNC_SOLTC_ESTRN,
                   HSOLTC_ESTRN,
                   CTPO_OPER_MOTVO,
                   CMOTVO_JUSTF,
                   RJUSTF_SOLTC_ESTRN,
                   CDEPDC_PAREC_ESTRN,
                   CFUNC_PAREC_ESTRN,
                   HPAREC_ESTRN,
                   RJUSTF_PAREC_ESTRN,
                   QESTRN_AGPTO,
                   VESTRN_SOLTD,
                   QCANCT_AGPTO,
                   VCANCT_SOLTD,
                   DEFETV_CREDT_ESTRN)
               VALUES
                  (:GFCTB0A7.CAGPTO-CTA,
                   :GFCTB0A7.CSERVC-TARIF,
                   :GFCTB0A7.DOCOR-EVNTO,
                   :GFCTB0A7.HIDTFD-ESTRN-AGPTO,
                   :GFCTB0A7.CSIT-SOLTC-ESTRN,
                   :GFCTB0A7.CSIT-PROCM-ESTRN,
                   :GFCTB0A7.CINDCD-AGPTO-TOT,
                   :GFCTB0A7.CDEPDC-SOLTC-ESTRN,
                   :GFCTB0A7.CFUNC-SOLTC-ESTRN,
                   :GFCTB0A7.HSOLTC-ESTRN,
                   :GFCTB0A7.CTPO-OPER-MOTVO,
                   :GFCTB0A7.CMOTVO-JUSTF,
                   :GFCTB0A7.RJUSTF-SOLTC-ESTRN,
                   :GFCTB0A7.CDEPDC-PAREC-ESTRN,
                   :GFCTB0A7.CFUNC-PAREC-ESTRN,
                   :GFCTB0A7.HPAREC-ESTRN,
                   :GFCTB0A7.RJUSTF-PAREC-ESTRN,
                   :GFCTB0A7.QESTRN-AGPTO,
                   :GFCTB0A7.VESTRN-SOLTD,
                   :GFCTB0A7.QCANCT-AGPTO,
                   :GFCTB0A7.VCANCT-SOLTD,
                   :GFCTB0A7.DEFETV-CREDT-ESTRN)
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE SPACES             TO GFCT0M-AREA-ERROS
               INITIALIZE              GFCT0M-AREA-ERROS
               MOVE 'PDIDO_ESTRN_GRP'  TO GFCT0M-NOME-TAB
               MOVE 'INSERT'           TO GFCT0M-COMANDO-SQL
               MOVE '0130'             TO GFCT0M-LOCAL
               MOVE 0003               TO GFCTFS-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB0A7'         TO WRK-NOME-TAB

               PERFORM 13110-MOVER-ERRO-SQL

               MOVE SPACES             TO GFCTFS-DADOS-RST

               PERFORM 30000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       29200-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       29300-GERAR-PAB                 SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTFR-CAGPTO          TO CAGPTO-CTA        OF GFCTB0A8.
           MOVE WRK-HORA               TO HIDTFD-ESTRN-AGPTO
                                                            OF GFCTB0A8.
           MOVE GFCTFR-CSERVC-TARIF    TO CSERVC-TARIF      OF GFCTB0A8.
           MOVE GFCTFR-DATA-DEBITO     TO DOCOR-EVNTO       OF GFCTB0A8.
           MOVE 237                    TO CEMPR-INC         OF GFCTB0A8.
           MOVE GFCTFR-CAGENCIA        TO CDEPDC            OF GFCTB0A8.
           MOVE GFCTFR-CPAB            TO CPOSTO-SERVC      OF GFCTB0A8.

           EXEC SQL
             INSERT INTO DB2PRD.PDIDO_ESTRN_PAB
                  (CAGPTO_CTA,
                   CSERVC_TARIF,
                   DOCOR_EVNTO,
                   HIDTFD_ESTRN_AGPTO,
                   CEMPR_INC,
                   CDEPDC,
                   CPOSTO_SERVC)
               VALUES
                  (:GFCTB0A8.CAGPTO-CTA,
                   :GFCTB0A8.CSERVC-TARIF,
                   :GFCTB0A8.DOCOR-EVNTO,
                   :GFCTB0A8.HIDTFD-ESTRN-AGPTO,
                   :GFCTB0A8.CEMPR-INC,
                   :GFCTB0A8.CDEPDC,
                   :GFCTB0A8.CPOSTO-SERVC)
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE SPACES             TO GFCT0M-AREA-ERROS
               INITIALIZE              GFCT0M-AREA-ERROS
               MOVE 'PDIDO_ESTRN_PAB'  TO GFCT0M-NOME-TAB
               MOVE 'INSERT'           TO GFCT0M-COMANDO-SQL
               MOVE '0140'             TO GFCT0M-LOCAL
               MOVE 0003               TO GFCTFS-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB0A8'         TO WRK-NOME-TAB

               PERFORM 13110-MOVER-ERRO-SQL

               MOVE SPACES             TO GFCTFS-DADOS-RST

               PERFORM 30000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       29300-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       29400-GERAR-AGENCIA             SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTFR-CAGPTO          TO CAGPTO-CTA        OF GFCTB0A3.
           MOVE WRK-HORA               TO HIDTFD-ESTRN-AGPTO
                                                            OF GFCTB0A3.
           MOVE GFCTFR-CSERVC-TARIF    TO CSERVC-TARIF      OF GFCTB0A3.
           MOVE GFCTFR-DATA-DEBITO     TO DOCOR-EVNTO       OF GFCTB0A3.
           MOVE 237                    TO CEMPR-INC         OF GFCTB0A3.
           MOVE GFCTFR-CAGENCIA        TO CDEPDC            OF GFCTB0A3.

           EXEC SQL
             INSERT INTO DB2PRD.PDIDO_ESTRN_AG
                  (CAGPTO_CTA,
                   CSERVC_TARIF,
                   DOCOR_EVNTO,
                   HIDTFD_ESTRN_AGPTO,
                   CEMPR_INC,
                   CDEPDC)
               VALUES
                  (:GFCTB0A3.CAGPTO-CTA,
                   :GFCTB0A3.CSERVC-TARIF,
                   :GFCTB0A3.DOCOR-EVNTO,
                   :GFCTB0A3.HIDTFD-ESTRN-AGPTO,
                   :GFCTB0A3.CEMPR-INC,
                   :GFCTB0A3.CDEPDC)
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE SPACES             TO GFCT0M-AREA-ERROS
               INITIALIZE              GFCT0M-AREA-ERROS
               MOVE 'PDIDO_ESTRN_AG '  TO GFCT0M-NOME-TAB
               MOVE 'INSERT'           TO GFCT0M-COMANDO-SQL
               MOVE '0150'             TO GFCT0M-LOCAL
               MOVE 0003               TO GFCTFS-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB0A3'         TO WRK-NOME-TAB

               PERFORM 13110-MOVER-ERRO-SQL

               MOVE SPACES             TO GFCTFS-DADOS-RST

               PERFORM 30000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       29400-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       29500-GERAR-EMPRESA             SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTFR-CAGPTO          TO CAGPTO-CTA        OF GFCTB0A6.
           MOVE WRK-HORA               TO HIDTFD-ESTRN-AGPTO
                                                            OF GFCTB0A6.
           MOVE GFCTFR-DATA-DEBITO     TO DOCOR-EVNTO       OF GFCTB0A6.
           MOVE GFCTFR-CSERVC-TARIF    TO CSERVC-TARIF      OF GFCTB0A6.
           MOVE GFCTFR-CAGENCIA        TO CAG-EMPR          OF GFCTB0A6.
           MOVE GFCTFR-CCONTA          TO CNRO-CTA-DEB      OF GFCTB0A6.
           MOVE ZEROS                  TO CRZ-CTA-EMPR      OF GFCTB0A6
                                          CLCTO-CTA-EMPR    OF GFCTB0A6.

           EXEC SQL
             INSERT INTO DB2PRD.PDIDO_ESTRN_EMPR
                  (CAGPTO_CTA,
                   CSERVC_TARIF,
                   DOCOR_EVNTO,
                   HIDTFD_ESTRN_AGPTO,
                   CAG_EMPR,
                   CRZ_CTA_EMPR,
                   CNRO_CTA_DEB,
                   CLCTO_CTA_EMPR)
               VALUES
                  (:GFCTB0A6.CAGPTO-CTA,
                   :GFCTB0A6.CSERVC-TARIF,
                   :GFCTB0A6.DOCOR-EVNTO,
                   :GFCTB0A6.HIDTFD-ESTRN-AGPTO,
                   :GFCTB0A6.CAG-EMPR,
                   :GFCTB0A6.CRZ-CTA-EMPR,
                   :GFCTB0A6.CNRO-CTA-DEB,
                   :GFCTB0A6.CLCTO-CTA-EMPR)
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE SPACES             TO GFCT0M-AREA-ERROS
               INITIALIZE              GFCT0M-AREA-ERROS
               MOVE 'PDIDO_ESTRN_EMPR' TO GFCT0M-NOME-TAB
               MOVE 'INSERT'           TO GFCT0M-COMANDO-SQL
               MOVE '0160'             TO GFCT0M-LOCAL
               MOVE 0003               TO GFCTFS-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB0A6'         TO WRK-NOME-TAB

               PERFORM 13110-MOVER-ERRO-SQL

               MOVE SPACES             TO GFCTFS-DADOS-RST

               PERFORM 30000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       29500-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       29600-GERAR-POSTAL              SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTFR-CAGPTO          TO CAGPTO-CTA        OF GFCTB0A9.
           MOVE WRK-HORA               TO HIDTFD-ESTRN-AGPTO
                                                            OF GFCTB0A9.
           MOVE GFCTFR-DATA-DEBITO     TO DOCOR-EVNTO       OF GFCTB0A9.
           MOVE GFCTFR-CSERVC-TARIF    TO CSERVC-TARIF      OF GFCTB0A9.
           MOVE 237                    TO CEMPR-INC         OF GFCTB0A9.
           MOVE GFCTFR-CAGENCIA        TO CDEPDC            OF GFCTB0A9.
           MOVE GFCTFR-CPAB            TO CPOSTO-SERVC      OF GFCTB0A9.

           EXEC SQL
             INSERT INTO DB2PRD.PDIDO_ESTRN_PSTAL
                  (CAGPTO_CTA,
                   CSERVC_TARIF,
                   DOCOR_EVNTO,
                   HIDTFD_ESTRN_AGPTO,
                   CEMPR_INC,
                   CDEPDC,
                   CPOSTO_SERVC)
               VALUES
                  (:GFCTB0A9.CAGPTO-CTA,
                   :GFCTB0A9.CSERVC-TARIF,
                   :GFCTB0A9.DOCOR-EVNTO,
                   :GFCTB0A9.HIDTFD-ESTRN-AGPTO,
                   :GFCTB0A9.CEMPR-INC,
                   :GFCTB0A9.CDEPDC,
                   :GFCTB0A9.CPOSTO-SERVC)
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE SPACES             TO GFCT0M-AREA-ERROS
               INITIALIZE              GFCT0M-AREA-ERROS
               MOVE 'PDIDO_ESTRN_PSTAL'
                                       TO GFCT0M-NOME-TAB
               MOVE 'INSERT'           TO GFCT0M-COMANDO-SQL
               MOVE '0170'             TO GFCT0M-LOCAL
               MOVE 0003               TO GFCTFS-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB0A9'         TO WRK-NOME-TAB

               PERFORM 13110-MOVER-ERRO-SQL

               MOVE SPACES             TO GFCTFS-DADOS-RST

               PERFORM 30000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       29600-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       29700-GERAR-CLIENTE             SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTFR-CAGPTO          TO CAGPTO-CTA        OF GFCTB0A4.
           MOVE WRK-HORA               TO HIDTFD-ESTRN-AGPTO
                                                            OF GFCTB0A4.
           MOVE GFCTFR-DATA-DEBITO     TO DOCOR-EVNTO       OF GFCTB0A4.
           MOVE GFCTFR-CSERVC-TARIF    TO CSERVC-TARIF      OF GFCTB0A4.
ST2506*    MOVE GFCTFR-CGC-CPF         TO CCGC-CPF          OF GFCTB0A4.
ST2506*    MOVE GFCTFR-FILIAL          TO CFLIAL-CGC        OF GFCTB0A4.
ST2506*    MOVE GFCTFR-CONTROLE        TO CCTRL-CPF-CGC     OF GFCTB0A4.
ST2506     MOVE GFCTFR-CGC-CPF         TO CCGC-CPF-ST       OF GFCTB0A4.
ST2506     MOVE GFCTFR-FILIAL          TO CFLIAL-CGC-ST     OF GFCTB0A4.
ST2506     MOVE GFCTFR-CONTROLE        TO CCTRL-CPF-CGC-ST  OF GFCTB0A4.

           EXEC SQL
             INSERT INTO DB2PRD.PDIDO_ESTRN_CLI
                  (CAGPTO_CTA,
                   CSERVC_TARIF,
                   DOCOR_EVNTO,
                   HIDTFD_ESTRN_AGPTO,
ST2506*            CCGC_CPF,
ST2506*            CFLIAL_CGC,
ST2506*            CCTRL_CPF_CGC,
ST2506             CCGC_CPF_ST,
ST2506             CFLIAL_CGC_ST,
ST2506             CCTRL_CPF_CGC_ST)
               VALUES
                  (:GFCTB0A4.CAGPTO-CTA,
                   :GFCTB0A4.CSERVC-TARIF,
                   :GFCTB0A4.DOCOR-EVNTO,
                   :GFCTB0A4.HIDTFD-ESTRN-AGPTO,
ST2506*            :GFCTB0A4.CCGC-CPF,
ST2506*            :GFCTB0A4.CFLIAL-CGC,
ST2506*            :GFCTB0A4.CCTRL-CPF-CGC)
ST2506             :GFCTB0A4.CCGC-CPF-ST,
ST2506             :GFCTB0A4.CFLIAL-CGC-ST,
ST2506             :GFCTB0A4.CCTRL-CPF-CGC-ST)
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE SPACES             TO GFCT0M-AREA-ERROS
               INITIALIZE              GFCT0M-AREA-ERROS
               MOVE 'PDIDO_ESTRN_CLI'  TO GFCT0M-NOME-TAB
               MOVE 'INSERT'           TO GFCT0M-COMANDO-SQL
               MOVE '0180'             TO GFCT0M-LOCAL
               MOVE 0003               TO GFCTFS-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB0A4'         TO WRK-NOME-TAB

               PERFORM 13110-MOVER-ERRO-SQL

               MOVE SPACES             TO GFCTFS-DADOS-RST

               PERFORM 30000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       29700-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       29800-GERAR-SEGMENTO            SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTFR-CAGPTO          TO CAGPTO-CTA        OF GFCTB0B0.
           MOVE WRK-HORA               TO HIDTFD-ESTRN-AGPTO
                                                            OF GFCTB0B0.
           MOVE GFCTFR-DATA-DEBITO     TO DOCOR-EVNTO       OF GFCTB0B0.
           MOVE GFCTFR-CSERVC-TARIF    TO CSERVC-TARIF      OF GFCTB0B0.
           MOVE GFCTFR-CSEGMENTO       TO CSGMTO-GSTAO-TARIF
                                                            OF GFCTB0B0.
           MOVE ZEROS                  TO WRK-CSGMTO-GSTAO-TARIF-NULL.

           EXEC SQL
             INSERT INTO DB2PRD.PDIDO_ESTRN_SGMTO
                  (CAGPTO_CTA,
                   CSERVC_TARIF,
                   DOCOR_EVNTO,
                   HIDTFD_ESTRN_AGPTO,
                   CSGMTO_GSTAO_TARIF)
               VALUES
                  (:GFCTB0B0.CAGPTO-CTA,
                   :GFCTB0B0.CSERVC-TARIF,
                   :GFCTB0B0.DOCOR-EVNTO,
                   :GFCTB0B0.HIDTFD-ESTRN-AGPTO,
                   :GFCTB0B0.CSGMTO-GSTAO-TARIF
                     :WRK-CSGMTO-GSTAO-TARIF-NULL)
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               MOVE SPACES             TO GFCT0M-AREA-ERROS
               INITIALIZE              GFCT0M-AREA-ERROS
               MOVE 'PDIDO_ESTRN_SGMTO'
                                       TO GFCT0M-NOME-TAB
               MOVE 'INSERT'           TO GFCT0M-COMANDO-SQL
               MOVE '0190'             TO GFCT0M-LOCAL
               MOVE 0003               TO GFCTFS-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               MOVE 'GFCTB0B0'         TO WRK-NOME-TAB

               PERFORM 13110-MOVER-ERRO-SQL

               MOVE SPACES             TO GFCTFS-DADOS-RST

               PERFORM 30000-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       29800-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       30000-FINALIZAR                 SECTION.
      *----------------------------------------------------------------*

           MOVE ZEROS                  TO RETURN-CODE.

           GOBACK.

      *----------------------------------------------------------------*
       90000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*
