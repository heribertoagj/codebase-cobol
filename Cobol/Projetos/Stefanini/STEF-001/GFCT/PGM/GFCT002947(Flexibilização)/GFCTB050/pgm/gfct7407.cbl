      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT7407.
       AUTHOR.     SIMONI FAVRETTO.
      *================================================================*
      *                    C P M  -  S I S T E M A S                   *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT7407                                    *
      *    PROGRAMADORA:   SIMONI FAVRETTO         - CPM PATO BRANCO   *
      *    ANALISTA CPM:   EDSON DOS SANTOS        - CPM PATO BRANCO   *
      *    ANALISTA....:   VALERIA                 - PROCKWORK GP 50   *
      *    DATA........:   05/05/2006                                  *
      *                                                                *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   EFETUAR COMUNICACAO ENTRE WEB E MF VIA      *
      *      SBAT3000 PARA AS CHAMADAS NOS MODULOS  REFERENTES  A      *
      *      FUNCIONALIDADE DE FLEXIBILIZACAO POR AGRUPAMENTO.         *
      *                                                                *
      *----------------------------------------------------------------*
      *    BOOK'S......:                                               *
      *    I#ISDDPC - AREA DE COMUNICACAO COM SBAT3000                 *
      *    I#GFCT0K - ENTRADA - SBAT3000                               *
      *    I#GFCT0L - SAIDA   - SBAT3000                               *
      *    I#GFCT0M - ERROS   - SBAT3000                               *
      *    I#GFCT3O - RESTART - SBAT3000                               *
      *    I#GFCTN9 - ENTRADA - INCLUSAO DE FLEXIBILIZACAO             *
      *    I#GFCTNA - SAIDA   - INCLUSAO DE FLEXIBILIZACAO             *
      *    I#GFCTNJ - ENTRADA - LISTA DE CONSULTA                      *
      *    I#GFCTNK - SAIDA   - LISTA DE CONSULTA                      *
      *    I#GFCTNL - ENTRADA - FUNCAO DE CONSULTA DETALHE             *
      *    I#GFCTNM - SAIDA   - FUNCAO DE CONSULTA DETALHE             *
      *    I#GFCTNB - ENTRADA - FUNCAO DE RENOVACAO                    *
      *    I#GFCTNC - SAIDA   - FUNCAO DE RENOVACAO                    *
      *    I#GFCTND - ENTRADA - LISTA DE CANCELAMENTO                  *
      *    I#GFCTNE - SAIDA   - LISTA DE CANCELAMENTO                  *
      *    I#GFCTOV - ENTRADA - FUNCAO DE CANCELAMENTO                 *
      *    I#GFCTOX - SAIDA   - FUNCAO DE CANCELAMENTO                 *
      *    I#GFCTNF - ENTRADA - LISTA DE ENCERRAMENTO                  *
      *    I#GFCTNG - SAIDA   - LISTA DE ENCERRAMENTO                  *
      *    I#GFCTNT - ENTRADA - FUNCAO DE ENCERRAMENTO                 *
      *    I#GFCTNU - SAIDA   - FUNCAO DE ENCERRAMENTO                 *
      *    I#GFCTNH - ENTRADA - FUNCAO DE LISTA DE VIGENTES            *
      *    I#GFCTNI - SAIDA   - FUNCAO DE LISTA DE VIGENTES            *
      *    I#GFCT3A - ENTRADA - COMBO DE TARIFAS                       *
      *    I#GFCT3B - SAIDA   - COMBO DE TARIFAS                       *
      *    I#GFCTCO - ENTRADA - TIPO DE OPERACAO                       *
      *    I#GFCTCP - SAIDA   - TIPO DE OPERACAO                       *
      *    I#GFCT3T - ENTRADA - MOTIVO DE FLEXIBILIZACAO               *
      *    I#GFCT3U - SAIDA   - MOTIVO DE FLEXIBILIZACAO               *
      *    I#GFCT1V - ENTRADA - TEXT NOME AGENCIA                      *
      *    I#GFCT1X - SAIDA   - TEXT NOME AGENCIA                      *
      *    I#GFCTGB - ENTRADA - TEXT POSTO / CORRESP BANCARIO          *
      *    I#GFCTGC - SAIDA   - TEXT POSTO / CORRESP BANCARIO          *
      *    I#GFCTKJ - ENTRADA - TEXT NOME MUNICIPIO                    *
      *    I#GFCTKK - SAIDA   - TEXT NOME MUNICIPIO                    *
      *    I#GFCTKL - ENTRADA - COMBO NOME UF                          *
      *    I#GFCTKM - SAIDA   - COMBO NOME UF                          *
      *    I#GFCTOK - ENTRADA - COMBO AGRUPTO DE FLEX                  *
      *    I#GFCTOL - SAIDA   - COMBO AGRUPTO DE FLEX                  *
      *    I#GFCTOM - ENTRADA - COMBO AGRUPTO FLEX AGRUPTO             *
      *    I#GFCTON - SAIDA   - COMBO AGRUPTO FLEX AGRUPTO             *
      *    I#GFCTOO - ENTRADA - COMBO TARIFAS FLEX POR AGRUPTO         *
      *    I#GFCTOP - SAIDA   - COMBO TARIFAS FLEX POR AGRUPTO         *
      *    I#GFCTG7 - ENTRADA - COMBO SEGMENTOS VIGENTES               *
      *    I#GFCTG8 - SAIDA   - COMBO SEGMENTOS VIGENTES               *
      *    I#GFCTOR - ENTRADA - COMBO SEGMENTOS COM FLEX               *
      *    I#GFCTOS - SAIDA   - COMBO SEGMENTOS COM FLEX               *
      *    I#GFCTOT - ENTRADA - COMBO UF COM FLEX AGRUP                *
      *    I#GFCTOU - SAIDA   - COMBO UF COM FLEX AGRUP                *
      *    I#GFCTOY - ENTRADA - TEXT NOME AGENCIA COM FLEX             *
      *    I#GFCTOZ - SAIDA   - TEXT NOME AGENCIA COM FLEX             *
      *    I#GFCTP0 - ENTRADA - TEXT POSTO   BANCARIO COM FLEX         *
      *    I#GFCTP1 - SAIDA   - TEXT POSTO   BANCARIO COM FLEX         *
      *    I#GFCTP2 - ENTRADA - TEXT CORRESP BANCARIO COM FLEX         *
      *    I#GFCTP3 - SAIDA   - TEXT CORRESP BANCARIO COM FLEX         *
      *    I#GFCTP4 - ENTRADA - TEXT NOME MUNICIPIO COM FLEX           *
      *    I#GFCTP5 - SAIDA   - TEXT NOME MUNICIPIO COM FLEX           *
      *    I#GFCTN9 - ENTRADA - COMBO DE CONJUNTO DE SERVICO           *
      *    I#GFCTNA - SAIDA   - COMBO DE CONJUNTO DE SERVICO           *
      *    I#GFCTVR - ENTRADA - COMBO DE TARIFAS                       *
      *    I#GFCTVS - SAIDA   - COMBO DE TARIFAS                       *
080708*    GFCTWAAM - ENTRADA - COMBO DE TARIFAS                       *
080708*    GFCTWAAN - SAIDA   - COMBO DE TARIFAS                       *
2912  *    GFCTWAJQ - ENTRADA - COMBO DE CONJUNTO DE SERVICOS HIST.    *
2912  *    GFCTWAJR - SAIDA   - COMBO DE CONJUNTO DE SERVICOS HIST.    *
2912  *    GFCTWAJU - ENTRADA - COMBO DE SEGMENTO HIST. FLEXZ.         *
2912  *    GFCTWAJV - SAIDA   - COMBO DE SEGMENTO HIST. FLEXZ.         *
2912  *    GFCTWAJY - ENTRADA - COMBO DE UF'S HIST. FLEXZ.             *
2912  *    GFCTWAJZ - SAIDA   - COMBO DE UF'S HIST. FLEXZ.             *
2912  *    GFCTWAJO - ENTRADA - COMBO DE CONJUNTO DE SERVICOS VIG.     *
2912  *    GFCTWAJP - SAIDA   - COMBO DE CONJUNTO DE SERVICOS VIG.     *
2912  *    GFCTWAJS - ENTRADA - COMBO DE SEGMENTO VIG.  FLEXZ.         *
2912  *    GFCTWAJT - SAIDA   - COMBO DE SEGMENTO VIG.  FLEXZ.         *
2912  *    GFCTWAJW - ENTRADA - COMBO DE UF'S VIG.  FLEXZ.             *
2912  *    GFCTWAJX - SAIDA   - COMBO DE UF'S VIG.  FLEXZ.             *
2912  *    I#GFCTIU - ENTRADA - COMBO NOME DO CLIENTE POR CPF/CNPJ     *
2912  *    I#GFCTIV - SAIDA   - COMBO NOME DO CLIENTE POR CPF/CNPJ     *
170610*    GFCTWAM0 - ENTRADA - COMBO CONJ SERVICO                     *
170610*    GFCTWAM1 - SAIDA   - COMBO CONJ SERVICO                     *
0610XS*    GFCTWAM2 - ENTRADA - TEXT  BANCO POSTAL / BRADESCO EXPRESSO *
0610XS*    GFCTWAM3 - SAIDA   - TEXT  BANCO POSTAL / BARDESCO EXPRESSO *
0710XS*    GFCTWANV - ENTRADA - TEXT  RAZAO CONTABIL (INCLUSAO)        *
0710XS*    GFCTWANW - SAIDA   - TEXT  RAZAO CONTABIL (INCLUSAO)        *
0710XS*    GFCTWANX - ENTRADA - COMBO RAZAO CONTABIL (RENOVACAO)       *
0710XS*    GFCTWANY - SAIDA   - COMBO RAZAO CONTABIL (RENOVACAO)       *
0710XS*    GFCTWANZ - ENTRADA - COMBO RAZAO CONTABIL (HISTORICO)       *
0710XS*    GFCTWAN0 - SAIDA   - COMBO RAZAO CONTABIL (HISTORICO)       *
0710XS*    GFCTWAN1 - ENTRADA - COMBO RAZAO CONTABIL (VIGENTE)         *
0710XS*    GFCTWAN2 - SAIDA   - COMBO RAZAO CONTABIL (VIGENTE)         *
      *                                                                *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    POOL0081 - ALOCAR DINAMICAMENTE UM MODULO                   *
      *    SBAT3000 - COMUNICACAO ENTRE PLATAFORMAS                    *
      *    GFCT0883 - LISTA DE CONSULTA DE FLEXIBILIZACAO POR AGRUP    *
      *    GFCT0408 - INCLUSAO DE FLEXIBILIZACAO                       *
      *    GFCT0418 - RENOVACAO DE FLEXIBILIZACAO                      *
      *    GFCT0420 - LISTA DE CANCELAMENTO DE FLEXIBILIZACAO          *
      *    GFCT0884 - CONSULTA DETALHE                                 *
      *    GFCT0886 - CANCELAMENTO DE FLEXIBILIZACAO                   *
      *    GFCT0882 - LISTA DE ENCERRAMENTO DE FLEXIBILIZACAO          *
      *    GFCT0887 - ENCERRAMENTO DE FLEXIBILIZACAO                   *
      *    GFCT0422 - LISTA DE VIGENTES                                *
      *    GFCT5015 - COMBO DE TARIFAS                                 *
      *    GFCT5068 - TIPO DE OPERACAO                                 *
      *    GFCT5006 - PESQUISA NOME CLIENTE                            *
      *    GFCT5014 - MOTIVO DE FLEXIBILIZACAO                         *
      *    GFCT5146 - COMBO DE AGRUPAMENTOS COM FLEX                   *
      *    GFCT5145 - COMBO DE AGRUPAMENTOS                            *
      *    GFCT5088 - COMBO DE SEGMENTOS VIGENTES                      *
      *    GFCT5109 - TEXT NOME POSTO BANCARIO                         *
      *    GFCT5090 - TEXT NOME CORRESPONDENTE BANCARIO                *
      *    GFCT5117 - TEXT NOME MUNICIPIO                              *
      *    GFCT5118 - TEXT NOME UF                                     *
      *    GFCT5148 - COMBO DE SEGMENTO COM FLEX POR AGRUPAMENTO       *
      *    GFCT5149 - COMBO DE UF COM FLEX POR AGRUPAMENTO             *
      *    GFCT5147 - COMBO DE TARIFAS DE FLEX POR AGRUPAMENTO         *
      *    GFCT5150 - TEXT NOME POSTO BANCARIO COM FLEXIBILIZACAO      *
      *    GFCT5151 - TEXT NOME CORRESP BANCARIO COM FLEXIBILIZACAO    *
      *    GFCT5152 - TEXT NOME MUNICIPIO FLEXIBILIZACAO               *
      *    GFCT5153 - TEXT NOME UF COM FLEXIBILIZACAO                  *
170610*    GFCT5294 - COMBO DE CONJUNTO DE SERVICO                     *
      *    GFCT5186 - COMBO DE TARIFAS                                 *
080708*    GFCT5588 - COMBO DE TARIFAS                                 *
2912  *    GFCT5169 - COMBO DE CONJUNTO DE SERVICOS (HISTORICO)        *
2912  *    GFCT5179 - COMBO DE SEGMENTO HIST. FLEXZ.                   *
2912  *    GFCT5207 - COMBO DE UF'S HIST. FLEXZ.                       *
2912  *    GFCT5166 - COMBO DE CONJUNTO DE SERVICOS (VIGENTES).        *
2912  *    GFCT5173 - COMBO DE SEGMENTO VIG.  FLEXZ.                   *
2912  *    GFCT5183 - COMBO DE UF'S VIG.  FLEXZ.                       *
2912  *    GFCT5110 - OBTER NOME DO CLIENTE POR CPF/CNPJ               *
0610XS*    GFCT5296 - VERIFICAR SE BANCO POSTAL OU BRADESCO EXPRESSO.  *
0710XS*    GFCT5298 - TEXT RAZAO CONTABIL (INCLUSAO).                  *
0710XS*    GFCT5299 - COMBO RAZAO CONTABIL (RENOVACAO).                *
0710XS*    GFCT5300 - COMBO RAZAO CONTABIL (HISTORICO).                *
0710XS*    GFCT5301 - COMBO RAZAO CONTABIL (VIGENTE).                  *
      *                                                                *
      *----------------------------------------------------------------*
      *    ULTIMA ALTERACAO EM 08/07/2008 - VALERIA                    *
      *    ALTERACOES REFERENTES AOS AGRUPAMENTOS PERSONALIZADOS       *
      *    - INCLUSAO COMBO TARIFAS VERIFICANDO PERMISSAO - GFCT5588   *
      *                                                                *
      *----------------------------------------------------------------*
2912  *    ULTIMA ALTERACAO EM 29/12/2009 - HELIO SANTONI              *
2912  *    ALTERACOES REFERENTES AS COMBOS DE AGRUPAMENTO              *
2912  *                                       (HISTORICO/VIGENTES).    *
2912  *    - INCLUSAO COMBO CONJUNTO DE SERVICOS                       *
2912  *    - INCLUSAO COMBO SEGMENTO                                   *
2912  *    - INCLUSAO COMBO UF'S                                       *
      *                                                                *
      *================================================================*
      *----------------------------------------------------------------*
0610XS*    ULTIMA ALTERACAO EM 25/06/2010 - SONDA PROCWORK             *
0610XS*    - INCLUSAO COMBO DE BANCO POSTAL / BRADESCO EXPRESSO        *
      *----------------------------------------------------------------*
0710XS*    ULTIMA ALTERACAO EM 07/07/2010 - SONDA PROCWORK             *
0710XS*    - INCLUSAO TEXT RAZAO CONTABIL (INCLUSAO)                   *
0710XS*    - INCLUSAO COMBO RAZAO CONTABIL (RENOVACAO)                 *
0710XS*    - INCLUSAO COMBO RAZAO CONTABIL (HISTORICO)                 *
0710XS*    - INCLUSAO COMBO RAZAO CONTABIL (VIGENTE)                   *
      *----------------------------------------------------------------*
BI1211*    ULTIMA ALTERACAO EM DEZ/2011   - SONDA PROCWORK             *
BI1211*    - PGM RECOMPILADO PARA ACATAR ALTERACAO DOS BOOKS:          *
BI1211*      I#GFCTNH E I#GFCTNI                                       *
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

      *---------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* INICIO DA WORKING STORAGE SECTION *'.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA INDEXADORES *'.
      *---------------------------------------------------------------*

       77  IND-1                       PIC  9(003) COMP-3  VALUE ZEROS.

      *---------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA VARIAVEIS AUXILIARES *'.
      *---------------------------------------------------------------*

       77  WRK-NRO-MSGI                PIC  9(002) COMP-3  VALUE ZEROS.
       77  WRK-MSGF                    PIC  X(004)         VALUE 'MSGF'.
       77  WRK-MSGI                    PIC  X(004)         VALUE 'MSGI'.
       77  WRK-RSTA                    PIC  X(004)         VALUE 'RSTA'.
       77  WRK-GU                      PIC  X(004)         VALUE 'GU'.
       77  WRK-ERRO                    PIC  X(004)         VALUE 'ERRO'.
       77  WRK-FUNCAO                  PIC  X(004)         VALUE SPACES.
       77  WRK-MODULO                  PIC  X(008)         VALUE SPACES.

      *---------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA MENSAGENS *'.
      *---------------------------------------------------------------*

       77  WRK-MSG01                   PIC  X(075)         VALUE
           'ERRO NO SBAT3000 PARA OBTER A MENSAGEM DE ENTRADA'.
       77  WRK-MSG02                   PIC  X(075)         VALUE
           'ERRO NO SBAT3000 PARA ENVIAR MENSAGEM - MSGI'.
       77  WRK-MSG03                   PIC  X(075)         VALUE
           'ERRO NO SBAT3000 PARA ENVIAR MENSAGEM - MSGF'.
       77  WRK-MSG04                   PIC  X(075)         VALUE
           'ERRO NO SBAT3000 PARA ENVIAR MENSAGEM - RSTA'.

       01  WRK-MSG05.
           05  FILLER                  PIC  X(026)         VALUE
               'ERRO NA CHAMADA DO MODULO '.
           05  WRK-MODULO-MSG          PIC  X(008)         VALUE SPACES.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* AREA PARA INCLUDES DO PROGRAMA *'.
      *---------------------------------------------------------------*

       COPY 'I#ISDDPC'.
       COPY 'I#GFCT0K'.
       COPY 'I#GFCT0L'.
       COPY 'I#GFCT0M'.
       COPY 'I#GFCT3O'.
       COPY 'I#GFCTN9'.
       COPY 'I#GFCTNA'.
       COPY 'I#GFCTNJ'.
       COPY 'I#GFCTNK'.
       COPY 'I#GFCTNL'.
       COPY 'I#GFCTNM'.
       COPY 'I#GFCTNB'.
       COPY 'I#GFCTNC'.
       COPY 'I#GFCTND'.
       COPY 'I#GFCTNE'.
       COPY 'I#GFCTOV'.
       COPY 'I#GFCTOX'.
       COPY 'I#GFCTNF'.
       COPY 'I#GFCTNG'.
       COPY 'I#GFCTNT'.
       COPY 'I#GFCTNU'.
       COPY 'I#GFCTNH'.
       COPY 'I#GFCTNI'.
       COPY 'I#GFCT3A'.
       COPY 'I#GFCT3B'.
       COPY 'I#GFCTCO'.
       COPY 'I#GFCTCP'.
       COPY 'I#GFCT3T'.
       COPY 'I#GFCT3U'.
       COPY 'I#GFCT1V'.
       COPY 'I#GFCT1X'.
       COPY 'I#GFCTGB'.
       COPY 'I#GFCTGC'.
       COPY 'I#GFCTKJ'.
       COPY 'I#GFCTKK'.
       COPY 'I#GFCTKL'.
       COPY 'I#GFCTKM'.
       COPY 'I#GFCTOK'.
       COPY 'I#GFCTOL'.
IN     COPY 'I#GFCTOM'.
       COPY 'I#GFCTON'.
       COPY 'I#GFCTOO'.
       COPY 'I#GFCTOP'.
       COPY 'I#GFCTG7'.
       COPY 'I#GFCTG8'.
       COPY 'I#GFCTOR'.
       COPY 'I#GFCTOS'.
       COPY 'I#GFCTOT'.
       COPY 'I#GFCTOU'.
       COPY 'I#GFCTOY'.
       COPY 'I#GFCTOZ'.
       COPY 'I#GFCTP0'.
       COPY 'I#GFCTP1'.
       COPY 'I#GFCTP2'.
       COPY 'I#GFCTP3'.
       COPY 'I#GFCTP4'.
       COPY 'I#GFCTP5'.
       COPY 'I#GFCTVR'.
       COPY 'I#GFCTVS'.
080708 COPY GFCTWAAM.
080708 COPY GFCTWAAN.
2912   COPY GFCTWAJQ.
2912   COPY GFCTWAJR.
2912   COPY GFCTWAJU.
2912   COPY GFCTWAJV.
2912   COPY GFCTWAJY.
2912   COPY GFCTWAJZ.
2912   COPY GFCTWAJO.
2912   COPY GFCTWAJP.
2912   COPY GFCTWAJS.
2912   COPY GFCTWAJT.
2912   COPY GFCTWAJW.
2912   COPY GFCTWAJX.
2912   COPY 'I#GFCTIU'.
2912   COPY 'I#GFCTIV'.

170610 COPY GFCTWAM0.
170610 COPY GFCTWAM1.

0610XS COPY GFCTWAM2.
0610XS COPY GFCTWAM3.

0710XS COPY GFCTWANV.
0710XS COPY GFCTWANW.

0710XS COPY GFCTWANX.
0710XS COPY GFCTWANY.

0710XS COPY GFCTWANZ.
0710XS COPY GFCTWAN0.

0710XS COPY GFCTWAN1.
0710XS COPY GFCTWAN2.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '* FIM DA WORKING STORAGE SECTION *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       01  IO-PCB.
           05  IO-TERM                 PIC  X(008).
           05  FILLER                  PIC  X(002).
           05  IO-STA                  PIC  X(002).
           05  FILLER                  PIC  X(012).
           05  IO-MODNAME              PIC  X(008).

       01  ALT-PCB.
           05  ALT-PCBNAME             PIC  X(008).
           05  FILLER                  PIC  X(002).
           05  ALT-STATUS              PIC  X(002).
           05  ALT-DATE                PIC S9(007) COMP-3.
           05  ALT-TIME                PIC S9(007) COMP-3.
           05  ALT-SEQNO               PIC S9(003) COMP.
           05  FILLER                  PIC  X(002).
           05  ALT-MODNAME             PIC  X(008).

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

           ENTRY 'DLITCBL'             USING IO-PCB
                                             ALT-PCB.

      ******************************************************************
      * INICIAR PROCESSAMENTO                                          *
      ******************************************************************
      *----------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                  WAJQ-ENTRADA
                                       WAJR-SAIDA
                                       WAJU-ENTRADA
                                       WAJV-SAIDA
                                       WAJY-ENTRADA
                                       WAJZ-SAIDA.

           PERFORM 1000-RECEBER-MENSAGEM.

           PERFORM 2000-PROCESSAR.

           GO TO 0000-INICIAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * RECEBER MENSAGEM                                               *
      ******************************************************************
      *----------------------------------------------------------------*
       1000-RECEBER-MENSAGEM           SECTION.
      *----------------------------------------------------------------*

           CALL 'SBAT3000'             USING WRK-GU
                                             IO-PCB
                                             ALT-PCB
                                             MCA-ISDPA
                                             GFCT0K-ENTRADA.

           IF  MPA-RCODE-API           EQUAL 'QC'
               GOBACK
           END-IF.

           IF  MPA-RCODE-API           NOT EQUAL SPACES
               MOVE WRK-MSG01          TO GFCT0M-TEXTO
               PERFORM 1100-FORMATAR-ERRO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * FORMATAR AREA DE ERRO                                          *
      ******************************************************************
      *----------------------------------------------------------------*
       1100-FORMATAR-ERRO              SECTION.
      *----------------------------------------------------------------*

           MOVE 'APL'                  TO GFCT0M-TIPO-ACESSO.
           MOVE 'GFCT7407'             TO GFCT0M-TRANSACAO.
           MOVE '02'                   TO GFCT0L-MPARCODE-APLIC.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * PROCESSAMENTO CENTRAL                                          *
      ******************************************************************
      *----------------------------------------------------------------*
       2000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           EVALUATE GFCT0K-FUNCAO

             WHEN 'GFCT0883'
               MOVE GFCT0K-ENTRADA     TO GFCTNJ-ENTRADA
               PERFORM 2100-PROCESSAR-LISTA-CONSULTA
                       VARYING IND-1   FROM 1 BY 1
                       UNTIL IND-1     GREATER 3 OR
                       GFCTNK-FIM      EQUAL 'S'

             WHEN 'GFCT0408'
               MOVE GFCT0K-ENTRADA     TO GFCTN9-ENTRADA
               PERFORM 2200-PROCESSAR-INCLUSAO

             WHEN 'GFCT0418'
               MOVE GFCT0K-ENTRADA     TO GFCTNB-ENTRADA
               PERFORM 2300-PROCESSAR-RENOVACAO-FLEX

             WHEN 'GFCT0420'
               MOVE GFCT0K-ENTRADA     TO GFCTND-ENTRADA
               PERFORM 2400-PROCESSAR-LISTA-CANCEL
                       VARYING IND-1   FROM 1 BY 1
                       UNTIL IND-1     GREATER 3 OR
                       GFCTNE-FIM      EQUAL 'S'

             WHEN 'GFCT0884'
               MOVE GFCT0K-ENTRADA     TO GFCTNL-ENTRADA
               PERFORM 2500-PROCESSAR-CONSULTA-DET

             WHEN 'GFCT5068'
               MOVE GFCT0K-ENTRADA     TO GFCTCO-ENTRADA
               PERFORM 2600-PROCESSAR-TIPO-OPERACAO
                       VARYING IND-1   FROM 1 BY 1
                       UNTIL IND-1     GREATER 3 OR
                       GFCTCP-FIM      EQUAL 'S'

             WHEN 'GFCT5014'
               MOVE GFCT0K-ENTRADA     TO GFCT3T-ENTRADA
               PERFORM 2700-PROCESSAR-MOTIVO
                       VARYING IND-1   FROM 1 BY 1
                       UNTIL IND-1     GREATER 3 OR
                       GFCT3U-FIM      EQUAL 'S'

             WHEN 'GFCT5006'
               MOVE GFCT0K-ENTRADA     TO GFCT1V-ENTRADA
               PERFORM 2800-PROCESSAR-TEXT-NOME-AGE

             WHEN 'GFCT5015'
               MOVE GFCT0K-ENTRADA     TO GFCT3A-ENTRADA
               PERFORM 2900-PROCESSAR-COMBO-TARIFA
                       VARYING IND-1   FROM 1 BY 1
                       UNTIL IND-1     GREATER 3 OR
                       GFCT3B-FIM      EQUAL 'S'

             WHEN 'GFCT5146'
               MOVE GFCT0K-ENTRADA     TO GFCTOM-ENTRADA
               PERFORM 3000-PROCESSAR-COMBO-AGP-FLEX
                       VARYING IND-1   FROM 1 BY 1
                       UNTIL IND-1     GREATER 3 OR
                       GFCTON-FIM      EQUAL 'S'

             WHEN 'GFCT0886'
               MOVE GFCT0K-ENTRADA     TO GFCTOV-ENTRADA
               PERFORM 3200-PROCESSAR-CANCEL-FLEX

             WHEN 'GFCT0882'
               MOVE GFCT0K-ENTRADA     TO GFCTNF-ENTRADA
               PERFORM 3300-PROCESSAR-LISTA-ENCERR
                       VARYING IND-1   FROM 1 BY 1
                       UNTIL IND-1     GREATER 3 OR
                       GFCTNG-FIM      EQUAL 'S'

             WHEN 'GFCT0887'
               MOVE GFCT0K-ENTRADA     TO GFCTNT-ENTRADA
               PERFORM 3400-PROCESSAR-ENCERR-FLEX

             WHEN 'GFCT0422'
               MOVE GFCT0K-ENTRADA     TO GFCTNH-ENTRADA
               PERFORM 3500-PROCESSAR-LISTA-VIG-FLEX
                       VARYING IND-1   FROM 1 BY 1
                       UNTIL IND-1     GREATER 3 OR
                       GFCTNI-FIM      EQUAL 'S'

             WHEN 'GFCT5145'
230816         MOVE GFCT0K-ENTRADA(1:100)
                                       TO GFCTOK-ENTRADA
               PERFORM 3600-PROCESSAR-COMBO-AGRUP
                       VARYING IND-1   FROM 1 BY 1
                       UNTIL IND-1     GREATER 3 OR
                       GFCTOL-FIM      EQUAL 'S'

             WHEN 'GFCT5088'
               MOVE GFCT0K-ENTRADA     TO GFCTG7-ENTRADA
               PERFORM 3700-PROCESSAR-COMBO-SEG-VIG
                       VARYING IND-1   FROM 1 BY 1
                       UNTIL IND-1     GREATER 3 OR
                       GFCTG8-FIM      EQUAL 'S'

             WHEN 'GFCT5109'
             WHEN 'GFCT5090'
               MOVE GFCT0K-ENTRADA     TO GFCTGB-ENTRADA
               PERFORM 3800-PROCESSAR-TEXT-POSTO-CORR

             WHEN 'GFCT5117'
               MOVE GFCT0K-ENTRADA     TO GFCTKJ-ENTRADA
               PERFORM 3900-PROCESSAR-TEXT-NOME-MUNIC

             WHEN 'GFCT5118'
               MOVE GFCT0K-ENTRADA     TO GFCTKL-ENTRADA
               PERFORM 4000-PROCESSAR-COMBO-NOME-UF
                       VARYING IND-1   FROM 1 BY 1
                       UNTIL IND-1     GREATER 3 OR
                       GFCTKM-FIM      EQUAL 'S'

             WHEN 'GFCT5148'
               MOVE GFCT0K-ENTRADA     TO GFCTOR-ENTRADA
               PERFORM 4200-PROCESSAR-COMBO-SEG-AGRUP
                       VARYING IND-1   FROM 1 BY 1
                       UNTIL IND-1     GREATER 3 OR
                       GFCTOS-FIM      EQUAL 'S'

             WHEN 'GFCT5149'
               MOVE GFCT0K-ENTRADA     TO GFCTOT-ENTRADA
               PERFORM 4300-PROCESSAR-COMBO-UF-AGRUP
                       VARYING IND-1   FROM 1 BY 1
                       UNTIL IND-1     GREATER 3 OR
                       GFCTOU-FIM      EQUAL 'S'

             WHEN 'GFCT5147'
               MOVE GFCT0K-ENTRADA     TO GFCTOO-ENTRADA
               PERFORM 4400-PROCESSAR-COMBO-TAR-AGP
                       VARYING IND-1   FROM 1 BY 1
                       UNTIL IND-1     GREATER 3 OR
                       GFCTOP-FIM      EQUAL 'S'

             WHEN 'GFCT5150'
               MOVE GFCT0K-ENTRADA     TO GFCTOY-ENTRADA
               PERFORM 4500-PROCESSAR-TEXT-NOME-AGE

             WHEN 'GFCT5151'
               MOVE GFCT0K-ENTRADA     TO GFCTP0-ENTRADA
               PERFORM 4600-PROCESSAR-TEXT-POSTO-FLEX

             WHEN 'GFCT5152'
               MOVE GFCT0K-ENTRADA     TO GFCTP2-ENTRADA
               PERFORM 4700-PROCESSAR-TEXT-CORRESP

             WHEN 'GFCT5153'
               MOVE GFCT0K-ENTRADA     TO GFCTP4-ENTRADA
               PERFORM 4800-PROCESSAR-TEXT-MUN-FLEX

070510       WHEN 'GFCT5294'
0710XS         MOVE GFCT0K-ENTRADA(1:100)
                                       TO WAM0-ENTRADA

               PERFORM 4900-PROCESSAR-COMBO-CJTO-SERV
                       VARYING IND-1   FROM 1 BY 1
                       UNTIL IND-1     GREATER 3 OR
                       WAM1-FIM      EQUAL 'S'

             WHEN 'GFCT5186'
               MOVE GFCT0K-ENTRADA     TO GFCTVR-ENTRADA
               PERFORM 5000-PROCESSAR-COMBO-TARIFA
                       VARYING IND-1   FROM 1 BY 1
                       UNTIL IND-1     GREATER 3 OR
                       GFCTVS-FIM      EQUAL 'S'

080708       WHEN 'GFCT5588'
230816         MOVE GFCT0K-ENTRADA(1:100)
080708                                 TO WAAM-ENTRADA
080708         PERFORM 5500-PROCESSAR-COMBO-TAR5588
080708                 VARYING IND-1   FROM 1 BY 1
080708                 UNTIL IND-1     GREATER 3 OR
080708                 WAAN-FIM        EQUAL 'S'

2912         WHEN 'GFCT5169'
0710XS         MOVE GFCT0K-ENTRADA(1:100)
2912                                   TO WAJQ-ENTRADA
2912
2912           PERFORM 5600-PROCESSA-COMBO-GFCT5169
2912                   VARYING IND-1   FROM 1 BY 1
2912                   UNTIL IND-1     GREATER 3 OR
2912                   WAJR-FIM        EQUAL 'S'

2912         WHEN 'GFCT5179'
0710XS         MOVE GFCT0K-ENTRADA(1:100)
2912                                   TO WAJU-ENTRADA
2912
2912           PERFORM 5700-PROCESSA-COMBO-GFCT5179
2912                   VARYING IND-1   FROM 1 BY 1
2912                   UNTIL IND-1     GREATER 3 OR
2912                   WAJV-FIM        EQUAL 'S'

2912         WHEN 'GFCT5207'
0710XS         MOVE GFCT0K-ENTRADA(1:100)
2912                                   TO WAJY-ENTRADA
2912
2912           PERFORM 5800-PROCESSA-COMBO-GFCT5207
2912                   VARYING IND-1   FROM 1 BY 1
2912                   UNTIL IND-1     GREATER 3 OR
2912                   WAJZ-FIM        EQUAL 'S'

2912         WHEN 'GFCT5166'
0710XS         MOVE GFCT0K-ENTRADA(1:100)
2912                                   TO WAJO-ENTRADA
2912
2912           PERFORM 5900-PROCESSA-COMBO-GFCT5166
2912                   VARYING IND-1   FROM 1 BY 1
2912                   UNTIL IND-1     GREATER 3 OR
2912                   WAJP-FIM        EQUAL 'S'

2912         WHEN 'GFCT5173'
0710XS         MOVE GFCT0K-ENTRADA(1:100)
2912                                   TO WAJS-ENTRADA
2912
2912           PERFORM 6100-PROCESSA-COMBO-GFCT5173
2912                   VARYING IND-1   FROM 1 BY 1
2912                   UNTIL IND-1     GREATER 3 OR
2912                   WAJT-FIM        EQUAL 'S'

2912         WHEN 'GFCT5183'
0710XS         MOVE GFCT0K-ENTRADA(1:100)
2912                                   TO WAJW-ENTRADA
2912
2912           PERFORM 6200-PROCESSA-COMBO-GFCT5183
2912                   VARYING IND-1   FROM 1 BY 1
2912                   UNTIL IND-1     GREATER 3 OR
2912                   WAJX-FIM        EQUAL 'S'

2912         WHEN 'GFCT5110'
0710XS         MOVE GFCT0K-ENTRADA(1:100)
2912                                   TO GFCTIU-ENTRADA
2912
2912           PERFORM 6300-PROCESSA-COMBO-GFCT5110
2912                   VARYING IND-1   FROM 1 BY 1
2912                   UNTIL IND-1     GREATER 3 OR
2912                   GFCTIV-FIM      EQUAL 'S'

0610XS       WHEN 'GFCT5296'
0710XS         MOVE GFCT0K-ENTRADA(1:100)
0610XS                                 TO WAM2-ENTRADA
0610XS
0610XS         PERFORM 6400-PROCESSAR-TEXT-POSTAL-EXP

0710XS       WHEN 'GFCT5298'
0710XS         MOVE GFCT0K-ENTRADA(1:100)
0710XS                                 TO WANV-ENTRADA
0710XS
0710XS         PERFORM 6500-PROCESSAR-TEXT-RZ-CONTAB

0710XS       WHEN 'GFCT5299'
0710XS         MOVE GFCT0K-ENTRADA(1:100)
0710XS                                 TO WANX-ENTRADA
0710XS
0710XS         PERFORM 6600-PROCESSAR-COMBO-RZ-CONTAB
0710XS                 VARYING IND-1   FROM 1 BY 1
0710XS                 UNTIL IND-1     GREATER 3 OR
0710XS                 WANY-FIM        EQUAL 'S'

0710XS       WHEN 'GFCT5300'
0710XS         MOVE GFCT0K-ENTRADA(1:100)
0710XS                                 TO WANZ-ENTRADA
0710XS
0710XS         PERFORM 6700-PROCESSAR-COMBO-RZ-CONTAB
0710XS                 VARYING IND-1   FROM 1 BY 1
0710XS                 UNTIL IND-1     GREATER 3 OR
0710XS                 WAN0-FIM        EQUAL 'S'

0710XS       WHEN 'GFCT5301'
0710XS         MOVE GFCT0K-ENTRADA(1:100)
0710XS                                 TO WAN1-ENTRADA
0710XS
0710XS         PERFORM 6800-PROCESSAR-COMBO-RZ-CONTAB
0710XS                 VARYING IND-1   FROM 1 BY 1
0710XS                 UNTIL IND-1     GREATER 3 OR
0710XS                 WAN2-FIM        EQUAL 'S'

           END-EVALUATE.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * PROCESSAR LISTA CONSULTA                                       *
      ******************************************************************
      *----------------------------------------------------------------*
       2100-PROCESSAR-LISTA-CONSULTA   SECTION.
      *----------------------------------------------------------------*

           IF  GFCTNJ-FIM              EQUAL 'N'
               MOVE SPACES             TO GFCTNK-SAIDA
                                          GFCT0M-AREA-ERROS
               INITIALIZE              GFCT0L-RET-WEB
                                       GFCTNK-SAIDA
                                       GFCT3O-AREA-RESTART
                                       GFCT0M-AREA-ERROS
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCTNJ-FUNCAO          TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTNJ-ENTRADA
                                             GFCTNK-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-MODULO-MSG
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               PERFORM 1100-FORMATAR-ERRO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  GFCTNK-ERRO             EQUAL 9
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           PERFORM 2110-TRATAR-RET-LISTA-CONSULTA.

           MOVE GFCTNK-SAIDA           TO GFCT0L-ENTRADA.

           IF  WRK-FUNCAO               EQUAL WRK-RSTA
               MOVE 255                 TO GFCT3O-LL
               MOVE GFCTNK-FUNCAO       TO GFCT3O-FUNCAO
               MOVE GFCTNK-FILTRO       TO GFCTNJ-FILTRO
               MOVE GFCTNK-QTDE-OCOR    TO GFCTNJ-QTDE-OCOR
               MOVE ZEROS               TO GFCTNJ-QTDE-TOT-REG
               MOVE GFCTNK-FUNC-BDSCO   TO GFCTNJ-FUNC-BDSCO
               MOVE GFCTNK-FIM          TO GFCTNJ-FIM
               MOVE GFCTNJ-DADOS-RST    TO GFCT3O-DADOS-RST
           END-IF.

           PERFORM 2120-RETORNAR-WEB.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * TRATAR RETORNO LISTA CONSULTA                                  *
      ******************************************************************
      *----------------------------------------------------------------*
       2110-TRATAR-RET-LISTA-CONSULTA  SECTION.
      *----------------------------------------------------------------*

           IF  GFCTNK-FIM              EQUAL 'S'
               MOVE WRK-MSGF           TO GFCT0L-TPOMSG-WEB
           ELSE
               IF  IND-1               LESS 3
                   MOVE WRK-MSGI       TO GFCT0L-TPOMSG-WEB
                   MOVE GFCTNK-QTDE-OCOR
                                       TO GFCTNJ-QTDE-OCOR
                   MOVE ZEROS          TO GFCTNJ-QTDE-TOT-REG
               ELSE
                   MOVE WRK-RSTA       TO GFCT0L-TPOMSG-WEB
               END-IF
           END-IF.

           IF  GFCT0L-TPOMSG-WEB       EQUAL WRK-RSTA OR WRK-MSGF
               MOVE ZEROS              TO GFCT0L-NRO-MSGI
           ELSE
230816         MOVE GFCT0L-NRO-MSGI    TO WRK-NRO-MSGI
230816         ADD 1                   TO WRK-NRO-MSGI
230816         MOVE WRK-NRO-MSGI       TO GFCT0L-NRO-MSGI
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCT0L-TPOMSG-WEB      TO WRK-FUNCAO.

      *----------------------------------------------------------------*
       2110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * RETORNAR PARA WEB                                              *
      ******************************************************************
      *----------------------------------------------------------------*
       2120-RETORNAR-WEB               SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FUNCAO              EQUAL WRK-RSTA
               CALL 'SBAT3000'         USING WRK-FUNCAO
                                             IO-PCB
                                             ALT-PCB
                                             MCA-ISDPA
                                             GFCT0L-ENTRADA
                                             GFCT3O-AREA-RESTART
           ELSE
               CALL 'SBAT3000'         USING WRK-FUNCAO
                                             IO-PCB
                                             ALT-PCB
                                             MCA-ISDPA
                                             GFCT0L-ENTRADA
           END-IF.

           IF  MPA-RCODE-API           NOT EQUAL SPACES
               IF  WRK-FUNCAO          EQUAL WRK-MSGI
                   MOVE WRK-MSG02      TO GFCT0M-TEXTO
               ELSE
                   IF  WRK-FUNCAO      EQUAL WRK-MSGF
                       MOVE WRK-MSG03  TO GFCT0M-TEXTO
                   ELSE
                       MOVE WRK-MSG04  TO GFCT0M-TEXTO
                   END-IF
               END-IF
               PERFORM 1100-FORMATAR-ERRO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       2120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * PROCESSAR INCLUSAO DE FLEXIBILIZACAO                           *
      ******************************************************************
      *----------------------------------------------------------------*
       2200-PROCESSAR-INCLUSAO         SECTION.
      *----------------------------------------------------------------*

           IF  GFCTN9-FIM              EQUAL 'N'
               MOVE SPACES             TO GFCTNA-SAIDA
                                          GFCT0M-AREA-ERROS
               INITIALIZE              GFCT0L-RET-WEB
                                       GFCTNA-SAIDA
                                       GFCT0M-AREA-ERROS
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCTN9-FUNCAO          TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTN9-ENTRADA
                                             GFCTNA-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-MODULO-MSG
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               PERFORM 1100-FORMATAR-ERRO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  GFCTNA-ERRO             EQUAL 9
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           PERFORM 2210-TRATAR-RETORNO-COMUM.

           MOVE GFCTNA-SAIDA           TO GFCT0L-ENTRADA.

           PERFORM 2120-RETORNAR-WEB.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * TRATAR RETORNO DOS MODULOS                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       2210-TRATAR-RETORNO-COMUM       SECTION.
      *----------------------------------------------------------------*

           MOVE ZEROS                  TO GFCT0L-NRO-MSGI.
           MOVE WRK-MSGF               TO GFCT0L-TPOMSG-WEB.
           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCT0L-TPOMSG-WEB      TO WRK-FUNCAO.

      *----------------------------------------------------------------*
       2210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * PROCESSAR RENOVACAO DE FLEXIBILIZACAO                          *
      ******************************************************************
      *----------------------------------------------------------------*
       2300-PROCESSAR-RENOVACAO-FLEX   SECTION.
      *----------------------------------------------------------------*

           IF  GFCTNB-FIM              EQUAL 'N'
               MOVE SPACES             TO GFCTNC-SAIDA
                                          GFCT0M-AREA-ERROS
               INITIALIZE              GFCT0L-RET-WEB
                                       GFCTNC-SAIDA
                                       GFCT0M-AREA-ERROS
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCTNB-FUNCAO          TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTNB-ENTRADA
                                             GFCTNC-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-MODULO-MSG
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               PERFORM 1100-FORMATAR-ERRO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  GFCTNC-ERRO             EQUAL 9
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           PERFORM 2210-TRATAR-RETORNO-COMUM.

           MOVE GFCTNC-SAIDA           TO GFCT0L-ENTRADA.

           PERFORM 2120-RETORNAR-WEB.

      *----------------------------------------------------------------*
       2300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * PROCESSAR LISTA DE CANCELAMENTO                                *
      ******************************************************************
      *----------------------------------------------------------------*
       2400-PROCESSAR-LISTA-CANCEL     SECTION.
      *----------------------------------------------------------------*

           IF  GFCTND-FIM              EQUAL 'N'
               MOVE SPACES             TO GFCTNE-SAIDA
                                          GFCT0M-AREA-ERROS
               INITIALIZE              GFCT0L-RET-WEB
                                       GFCTNE-SAIDA
                                       GFCT3O-AREA-RESTART
                                       GFCT0M-AREA-ERROS
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCTND-FUNCAO          TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTND-ENTRADA
                                             GFCTNE-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-MODULO-MSG
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               PERFORM 1100-FORMATAR-ERRO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  GFCTNE-ERRO             EQUAL 9
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           PERFORM 2410-TRATAR-RET-LISTA-CANCEL.

           MOVE GFCTNE-SAIDA           TO GFCT0L-ENTRADA.

           IF  WRK-FUNCAO               EQUAL WRK-RSTA
               MOVE 255                 TO GFCT3O-LL
               MOVE GFCTNE-QTDE-OCOR    TO GFCTND-QTDE-OCOR
               MOVE ZEROS               TO GFCTND-QTDE-TOT-REG
               MOVE GFCTNE-FUNC-BDSCO   TO GFCTND-FUNC-BDSCO
               MOVE GFCTNE-FUNCAO       TO GFCT3O-FUNCAO
               MOVE GFCTNE-FILTRO       TO GFCTND-FILTRO
               MOVE GFCTNE-PONTEIRO     TO GFCTND-PONTEIRO
               MOVE GFCTNE-FIM          TO GFCTND-FIM
               MOVE GFCTND-DADOS-RST    TO GFCT3O-DADOS-RST
           END-IF.

           PERFORM 2120-RETORNAR-WEB.

      *----------------------------------------------------------------*
       2400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * TRATAR RETORNO LISTA DE CANCELAMENTO                           *
      ******************************************************************
      *----------------------------------------------------------------*
       2410-TRATAR-RET-LISTA-CANCEL    SECTION.
      *----------------------------------------------------------------*

           IF  GFCTNE-FIM              EQUAL 'S'
               MOVE WRK-MSGF           TO GFCT0L-TPOMSG-WEB
           ELSE
               IF  IND-1               LESS 3
                   MOVE WRK-MSGI       TO GFCT0L-TPOMSG-WEB
                   MOVE GFCTNE-QTDE-OCOR
                                       TO GFCTND-QTDE-OCOR
                   MOVE ZEROS          TO GFCTND-QTDE-TOT-REG
               ELSE
                   MOVE WRK-RSTA       TO GFCT0L-TPOMSG-WEB
               END-IF
           END-IF.

           IF  GFCT0L-TPOMSG-WEB       EQUAL WRK-RSTA OR WRK-MSGF
               MOVE ZEROS              TO GFCT0L-NRO-MSGI
           ELSE
230816         MOVE GFCT0L-NRO-MSGI    TO WRK-NRO-MSGI
230816         ADD 1                   TO WRK-NRO-MSGI
230816         MOVE WRK-NRO-MSGI       TO GFCT0L-NRO-MSGI
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCT0L-TPOMSG-WEB      TO WRK-FUNCAO.

      *----------------------------------------------------------------*
       2410-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * PROCESSAR CONSULTA DETALHADA                                   *
      ******************************************************************
      *----------------------------------------------------------------*
       2500-PROCESSAR-CONSULTA-DET     SECTION.
      *----------------------------------------------------------------*

           IF  GFCTNL-FIM              EQUAL 'N'
               MOVE SPACES             TO GFCTNM-SAIDA
                                          GFCT0M-AREA-ERROS
               INITIALIZE              GFCT0L-RET-WEB
                                       GFCTNM-SAIDA
                                       GFCT0M-AREA-ERROS
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCTNL-FUNCAO          TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTNL-ENTRADA
                                             GFCTNM-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-MODULO-MSG
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               PERFORM 1100-FORMATAR-ERRO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  GFCTNM-ERRO             EQUAL 9
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           PERFORM 2210-TRATAR-RETORNO-COMUM.

           MOVE GFCTNM-SAIDA           TO GFCT0L-ENTRADA.

           PERFORM 2120-RETORNAR-WEB.

      *----------------------------------------------------------------*
       2500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * PROCESSAR TIPO DE OPERACAO                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       2600-PROCESSAR-TIPO-OPERACAO    SECTION.
      *----------------------------------------------------------------*

           IF  GFCTCO-FIM              EQUAL 'N'
               MOVE SPACES             TO GFCTCP-SAIDA
                                          GFCT0M-AREA-ERROS
               INITIALIZE              GFCT0L-RET-WEB
                                       GFCTCP-SAIDA
                                       GFCT3O-AREA-RESTART
                                       GFCT0M-AREA-ERROS
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCTCO-FUNCAO          TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTCO-ENTRADA
                                             GFCTCP-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-MODULO-MSG
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               PERFORM 1100-FORMATAR-ERRO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  GFCTCP-ERRO             EQUAL 9
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           PERFORM 2610-TRATAR-RET-TIPO-OPERACAO.

           MOVE GFCTCP-SAIDA           TO GFCT0L-ENTRADA.

           IF  WRK-FUNCAO               EQUAL WRK-RSTA
               MOVE 255                 TO GFCT3O-LL
               MOVE GFCTCP-FUNCAO       TO GFCT3O-FUNCAO
               MOVE GFCTCP-FILTRO       TO GFCTCO-FILTRO
               MOVE GFCTCP-QTDE-OCOR    TO GFCTCO-QTDE-OCOR
               MOVE ZEROS               TO GFCTCO-QTDE-TOT-REG
               MOVE GFCTCP-FUNC-BDSCO   TO GFCTCO-FUNC-BDSCO
               MOVE GFCTCP-PONTEIRO     TO GFCTCO-PONTEIRO
               MOVE GFCTCP-FIM          TO GFCTCO-FIM
               MOVE GFCTCO-DADOS-RST    TO GFCT3O-DADOS-RST
           END-IF.

           PERFORM 2120-RETORNAR-WEB.

      *----------------------------------------------------------------*
       2600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * TRATAR RETORNO TIPO DE OPERACAO                                *
      ******************************************************************
      *----------------------------------------------------------------*
       2610-TRATAR-RET-TIPO-OPERACAO   SECTION.
      *----------------------------------------------------------------*

           IF  GFCTCP-FIM              EQUAL 'S'
               MOVE WRK-MSGF           TO GFCT0L-TPOMSG-WEB
           ELSE
               IF  IND-1               LESS 3
                   MOVE WRK-MSGI       TO GFCT0L-TPOMSG-WEB
                   MOVE GFCTCP-PONTEIRO TO GFCTCO-PONTEIRO
               ELSE
                   MOVE WRK-RSTA       TO GFCT0L-TPOMSG-WEB
               END-IF
           END-IF.

           IF  GFCT0L-TPOMSG-WEB       EQUAL WRK-RSTA OR WRK-MSGF
               MOVE ZEROS              TO GFCT0L-NRO-MSGI
           ELSE
230816         MOVE GFCT0L-NRO-MSGI    TO WRK-NRO-MSGI
230816         ADD 1                   TO WRK-NRO-MSGI
230816         MOVE WRK-NRO-MSGI       TO GFCT0L-NRO-MSGI
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCT0L-TPOMSG-WEB      TO WRK-FUNCAO.

      *----------------------------------------------------------------*
       2610-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * PROCESSAR MOTIVO                                               *
      ******************************************************************
      *----------------------------------------------------------------*
       2700-PROCESSAR-MOTIVO           SECTION.
      *----------------------------------------------------------------*

           IF  GFCT3T-FIM              EQUAL 'N'
               MOVE SPACES             TO GFCT3U-SAIDA
                                          GFCT0M-AREA-ERROS
               INITIALIZE              GFCT0L-RET-WEB
                                       GFCT3U-SAIDA
                                       GFCT3O-AREA-RESTART
                                       GFCT0M-AREA-ERROS
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCT3T-FUNCAO          TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCT3T-ENTRADA
                                             GFCT3U-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-MODULO-MSG
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               PERFORM 1100-FORMATAR-ERRO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  GFCT3U-ERRO             EQUAL 9
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           PERFORM 2710-TRATAR-RET-MOTIVO.

           MOVE GFCT3U-SAIDA           TO GFCT0L-ENTRADA.

           IF  WRK-FUNCAO               EQUAL WRK-RSTA
               MOVE 255                 TO GFCT3O-LL
               MOVE GFCT3U-FUNCAO       TO GFCT3O-FUNCAO
               MOVE GFCT3U-FILTRO       TO GFCT3T-FILTRO
               MOVE GFCT3U-QTDE-OCOR    TO GFCT3T-QTDE-OCOR
               MOVE ZEROS               TO GFCT3T-QTDE-TOT-REG
               MOVE GFCT3U-PONTEIRO     TO GFCT3T-PONTEIRO
               MOVE GFCT3U-FUNC-BDSCO   TO GFCT3T-FUNC-BDSCO
               MOVE GFCT3U-FIM          TO GFCT3T-FIM
               MOVE GFCT3T-DADOS-RST    TO GFCT3O-DADOS-RST
           END-IF.

           PERFORM 2120-RETORNAR-WEB.

      *----------------------------------------------------------------*
       2700-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * TRATAR RETORNO MOTIVO                                          *
      ******************************************************************
      *----------------------------------------------------------------*
       2710-TRATAR-RET-MOTIVO          SECTION.
      *----------------------------------------------------------------*

           IF  GFCT3U-FIM              EQUAL 'S'
               MOVE WRK-MSGF           TO GFCT0L-TPOMSG-WEB
           ELSE
               IF  IND-1               LESS 3
                   MOVE WRK-MSGI       TO GFCT0L-TPOMSG-WEB
                   MOVE GFCT3U-PONTEIRO TO GFCT3T-PONTEIRO
               ELSE
                   MOVE WRK-RSTA       TO GFCT0L-TPOMSG-WEB
               END-IF
           END-IF.

           IF  GFCT0L-TPOMSG-WEB       EQUAL WRK-RSTA OR WRK-MSGF
               MOVE ZEROS              TO GFCT0L-NRO-MSGI
           ELSE
230816         MOVE GFCT0L-NRO-MSGI    TO WRK-NRO-MSGI
230816         ADD 1                   TO WRK-NRO-MSGI
230816         MOVE WRK-NRO-MSGI       TO GFCT0L-NRO-MSGI
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCT0L-TPOMSG-WEB      TO WRK-FUNCAO.

      *----------------------------------------------------------------*
       2710-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * PROCESSAR TEXT NOME AGENCIA                                    *
      ******************************************************************
      *----------------------------------------------------------------*
       2800-PROCESSAR-TEXT-NOME-AGE    SECTION.
      *----------------------------------------------------------------*

           IF  GFCT1V-FIM              EQUAL 'N'
               MOVE SPACES             TO GFCT1X-SAIDA
                                          GFCT0M-AREA-ERROS
               INITIALIZE              GFCT0L-RET-WEB
                                       GFCT1X-SAIDA
                                       GFCT0M-AREA-ERROS
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCT1V-FUNCAO          TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCT1V-ENTRADA
                                             GFCT1X-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-MODULO-MSG
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               PERFORM 1100-FORMATAR-ERRO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  GFCT1X-ERRO             EQUAL 9
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           PERFORM 2210-TRATAR-RETORNO-COMUM.

           MOVE GFCT1X-SAIDA           TO GFCT0L-ENTRADA.

           PERFORM 2120-RETORNAR-WEB.

      *----------------------------------------------------------------*
       2800-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * PROCESSAR COMBO TARIFA                                         *
      ******************************************************************
      *----------------------------------------------------------------*
       2900-PROCESSAR-COMBO-TARIFA     SECTION.
      *----------------------------------------------------------------*

           IF  GFCT3A-FIM              EQUAL 'N'
               MOVE SPACES             TO GFCT3B-SAIDA
                                          GFCT0M-AREA-ERROS
               INITIALIZE              GFCT0L-RET-WEB
                                       GFCT3B-SAIDA
                                       GFCT3O-AREA-RESTART
                                       GFCT0M-AREA-ERROS
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCT3A-FUNCAO          TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCT3A-ENTRADA
                                             GFCT3B-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-MODULO-MSG
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               PERFORM 1100-FORMATAR-ERRO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  GFCT3B-ERRO             EQUAL 9
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           PERFORM 2910-TRATAR-RET-COMBO-TARIFAS.

           MOVE GFCT3B-SAIDA           TO GFCT0L-ENTRADA.

           IF  WRK-FUNCAO               EQUAL WRK-RSTA
               MOVE 255                 TO GFCT3O-LL
               MOVE GFCT3B-FUNCAO       TO GFCT3O-FUNCAO
               MOVE GFCT3B-FILTRO       TO GFCT3A-FILTRO
               MOVE GFCT3B-QTDE-OCOR    TO GFCT3A-QTDE-OCOR
               MOVE ZEROS               TO GFCT3A-QTDE-TOT-REG
               MOVE GFCT3B-PONTEIRO     TO GFCT3A-PONTEIRO
               MOVE GFCT3B-FUNC-BDSCO   TO GFCT3A-FUNC-BDSCO
               MOVE GFCT3B-FIM          TO GFCT3A-FIM
               MOVE GFCT3A-DADOS-RST    TO GFCT3O-DADOS-RST
           END-IF.

           PERFORM 2120-RETORNAR-WEB.

      *----------------------------------------------------------------*
       2900-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * TRATAR RETORNO COMBO TARIFAS                                   *
      ******************************************************************
      *----------------------------------------------------------------*
       2910-TRATAR-RET-COMBO-TARIFAS   SECTION.
      *----------------------------------------------------------------*

           IF  GFCT3B-FIM              EQUAL 'S'
               MOVE WRK-MSGF           TO GFCT0L-TPOMSG-WEB
           ELSE
               IF  IND-1               LESS 3
                   MOVE WRK-MSGI       TO GFCT0L-TPOMSG-WEB
                   MOVE GFCT3B-PONTEIRO TO GFCT3A-PONTEIRO
               ELSE
                   MOVE WRK-RSTA       TO GFCT0L-TPOMSG-WEB
               END-IF
           END-IF.

           IF  GFCT0L-TPOMSG-WEB       EQUAL WRK-RSTA OR WRK-MSGF
               MOVE ZEROS              TO GFCT0L-NRO-MSGI
           ELSE
230816         MOVE GFCT0L-NRO-MSGI    TO WRK-NRO-MSGI
230816         ADD 1                   TO WRK-NRO-MSGI
230816         MOVE WRK-NRO-MSGI       TO GFCT0L-NRO-MSGI
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCT0L-TPOMSG-WEB      TO WRK-FUNCAO.

      *----------------------------------------------------------------*
       2910-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * PROCESSAR COMBO AGRUPAMENTOS COM FLEXIBILIZACAO                *
      ******************************************************************
      *----------------------------------------------------------------*
       3000-PROCESSAR-COMBO-AGP-FLEX   SECTION.
      *----------------------------------------------------------------*

           IF  GFCTOM-FIM              EQUAL 'N'
               MOVE SPACES             TO GFCTON-SAIDA
                                          GFCT0M-AREA-ERROS
               INITIALIZE              GFCT0L-RET-WEB
                                       GFCTON-SAIDA
                                       GFCT3O-AREA-RESTART
                                       GFCT0M-AREA-ERROS
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCTOM-FUNCAO          TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTOM-ENTRADA
                                             GFCTON-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-MODULO-MSG
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               PERFORM 1100-FORMATAR-ERRO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  GFCTON-ERRO             EQUAL 9
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           PERFORM 3100-TRATAR-RET-COMBO-AGP-FLEX.

           MOVE GFCTON-SAIDA           TO GFCT0L-ENTRADA.

           IF  WRK-FUNCAO               EQUAL WRK-RSTA
               MOVE 255                 TO GFCT3O-LL
               MOVE GFCTON-FUNCAO       TO GFCT3O-FUNCAO
               MOVE GFCTON-FILTRO       TO GFCTOM-FILTRO
               MOVE GFCTON-QTDE-OCOR    TO GFCTOM-QTDE-OCOR
               MOVE ZEROS               TO GFCTOM-QTDE-TOT-REG
               MOVE GFCTON-PONTEIRO     TO GFCTOM-PONTEIRO
               MOVE GFCTON-FUNC-BDSCO   TO GFCTOM-FUNC-BDSCO
               MOVE GFCTON-FIM          TO GFCTOM-FIM
               MOVE GFCTOM-DADOS-RST    TO GFCT3O-DADOS-RST
           END-IF.

           PERFORM 2120-RETORNAR-WEB.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * TRATAR RETORNO COMBO AGRUPAMENTOS COM FLEXIBILIZACAO           *
      ******************************************************************
      *----------------------------------------------------------------*
       3100-TRATAR-RET-COMBO-AGP-FLEX  SECTION.
      *----------------------------------------------------------------*

           IF  GFCTON-FIM              EQUAL 'S'
               MOVE WRK-MSGF           TO GFCT0L-TPOMSG-WEB
           ELSE
               IF  IND-1               LESS 3
                   MOVE WRK-MSGI       TO GFCT0L-TPOMSG-WEB
                   MOVE GFCTON-PONTEIRO
                                       TO GFCTOM-PONTEIRO
               ELSE
                   MOVE WRK-RSTA       TO GFCT0L-TPOMSG-WEB
               END-IF
           END-IF.

           IF  GFCT0L-TPOMSG-WEB       EQUAL WRK-RSTA OR WRK-MSGF
               MOVE ZEROS              TO GFCT0L-NRO-MSGI
           ELSE
230816         MOVE GFCT0L-NRO-MSGI    TO WRK-NRO-MSGI
230816         ADD 1                   TO WRK-NRO-MSGI
230816         MOVE WRK-NRO-MSGI       TO GFCT0L-NRO-MSGI
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCT0L-TPOMSG-WEB      TO WRK-FUNCAO.

      *----------------------------------------------------------------*
       3100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * PROCESSAR CANCELAMENTO DE FLEXIBILIZACAO                       *
      ******************************************************************
      *----------------------------------------------------------------*
       3200-PROCESSAR-CANCEL-FLEX      SECTION.
      *----------------------------------------------------------------*

           IF  GFCTOV-FIM              EQUAL 'N'
               MOVE SPACES             TO GFCTOX-SAIDA
                                          GFCT0M-AREA-ERROS
               INITIALIZE              GFCT0L-RET-WEB
                                       GFCTOX-SAIDA
                                       GFCT0M-AREA-ERROS
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCTOV-FUNCAO          TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTOV-ENTRADA
                                             GFCTOX-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-MODULO-MSG
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               PERFORM 1100-FORMATAR-ERRO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  GFCTOX-ERRO             EQUAL 9
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           PERFORM 2210-TRATAR-RETORNO-COMUM.

           MOVE GFCTOX-SAIDA           TO GFCT0L-ENTRADA.

           PERFORM 2120-RETORNAR-WEB.

      *----------------------------------------------------------------*
       3200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * PROCESSAR LISTA ENCERRAMENTO                                   *
      ******************************************************************
      *----------------------------------------------------------------*
       3300-PROCESSAR-LISTA-ENCERR     SECTION.
      *----------------------------------------------------------------*

           IF  GFCTNF-FIM              EQUAL 'N'
               MOVE SPACES             TO GFCTNG-SAIDA
                                          GFCT0M-AREA-ERROS
               INITIALIZE              GFCT0L-RET-WEB
                                       GFCTNG-SAIDA
                                       GFCT3O-AREA-RESTART
                                       GFCT0M-AREA-ERROS
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCTNF-FUNCAO          TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTNF-ENTRADA
                                             GFCTNG-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-MODULO-MSG
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               PERFORM 1100-FORMATAR-ERRO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  GFCTNG-ERRO             EQUAL 9
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           PERFORM 3310-TRATAR-RET-LISTA-ENCERR.

           MOVE GFCTNG-SAIDA           TO GFCT0L-ENTRADA.

           IF  WRK-FUNCAO               EQUAL WRK-RSTA
               MOVE 255                 TO GFCT3O-LL
               MOVE GFCTNG-FUNCAO       TO GFCT3O-FUNCAO
               MOVE GFCTNG-FILTRO       TO GFCTNF-FILTRO
               MOVE GFCTNG-QTDE-OCOR    TO GFCTNF-QTDE-OCOR
               MOVE ZEROS               TO GFCTNF-QTDE-TOT-REG
               MOVE GFCTNG-FUNC-BDSCO   TO GFCTNF-FUNC-BDSCO
               MOVE GFCTNG-PONTEIRO     TO GFCTNF-PONTEIRO
               MOVE GFCTNG-FIM          TO GFCTNF-FIM
               MOVE GFCTNF-DADOS-RST    TO GFCT3O-DADOS-RST
           END-IF.

           PERFORM 2120-RETORNAR-WEB.

      *----------------------------------------------------------------*
       3300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * TRATAR RETORNO LISTA DE ENCERRAMENTO                           *
      ******************************************************************
      *----------------------------------------------------------------*
       3310-TRATAR-RET-LISTA-ENCERR    SECTION.
      *----------------------------------------------------------------*

           IF  GFCTNG-FIM              EQUAL 'S'
               MOVE WRK-MSGF           TO GFCT0L-TPOMSG-WEB
           ELSE
               IF  IND-1               LESS 3
                   MOVE WRK-MSGI       TO GFCT0L-TPOMSG-WEB
                   MOVE GFCTNG-QTDE-OCOR
                                       TO GFCTNF-QTDE-OCOR
                   MOVE ZEROS          TO GFCTNF-QTDE-TOT-REG
               ELSE
                   MOVE WRK-RSTA       TO GFCT0L-TPOMSG-WEB
               END-IF
           END-IF.

           IF  GFCT0L-TPOMSG-WEB       EQUAL WRK-RSTA OR WRK-MSGF
               MOVE ZEROS              TO GFCT0L-NRO-MSGI
           ELSE
230816         MOVE GFCT0L-NRO-MSGI    TO WRK-NRO-MSGI
230816         ADD 1                   TO WRK-NRO-MSGI
230816         MOVE WRK-NRO-MSGI       TO GFCT0L-NRO-MSGI
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCT0L-TPOMSG-WEB      TO WRK-FUNCAO.

      *----------------------------------------------------------------*
       3310-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * PROCESSAR ENCERRAMENTO DE FLEXIBILIZACAO                       *
      ******************************************************************
      *----------------------------------------------------------------*
       3400-PROCESSAR-ENCERR-FLEX      SECTION.
      *----------------------------------------------------------------*

           IF  GFCTNT-FIM              EQUAL 'N'
               MOVE SPACES             TO GFCTNU-SAIDA
                                          GFCT0M-AREA-ERROS
               INITIALIZE              GFCT0L-RET-WEB
                                       GFCTNU-SAIDA
                                       GFCT0M-AREA-ERROS
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCTNT-FUNCAO          TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTNT-ENTRADA
                                             GFCTNU-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-MODULO-MSG
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               PERFORM 1100-FORMATAR-ERRO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  GFCTNU-ERRO             EQUAL 9
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           PERFORM 2210-TRATAR-RETORNO-COMUM.

           MOVE GFCTNU-SAIDA           TO GFCT0L-ENTRADA.

           PERFORM 2120-RETORNAR-WEB.

      *----------------------------------------------------------------*
       3400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * PROCESSAR LISTA DE VIGENTES DE FLEXIBILIZACAO                  *
      ******************************************************************
      *----------------------------------------------------------------*
       3500-PROCESSAR-LISTA-VIG-FLEX   SECTION.
      *----------------------------------------------------------------*

           IF  GFCTNH-FIM              EQUAL 'N'
               MOVE SPACES             TO GFCTNI-SAIDA
                                          GFCT0M-AREA-ERROS
               INITIALIZE              GFCT0L-RET-WEB
                                       GFCTNI-SAIDA
                                       GFCT3O-AREA-RESTART
                                       GFCT0M-AREA-ERROS
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCTNH-FUNCAO          TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTNH-ENTRADA
                                             GFCTNI-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-MODULO-MSG
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               PERFORM 1100-FORMATAR-ERRO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  GFCTNI-ERRO             EQUAL 9
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           PERFORM 3510-TRATAR-RET-LISTA-VIG-FLEX.

           MOVE GFCTNI-SAIDA           TO GFCT0L-ENTRADA.

           IF  WRK-FUNCAO               EQUAL WRK-RSTA
               MOVE 255                 TO GFCT3O-LL
               MOVE GFCTNI-FUNCAO       TO GFCT3O-FUNCAO
               MOVE GFCTNI-FILTRO       TO GFCTNH-FILTRO
               MOVE GFCTNI-QTDE-OCOR    TO GFCTNH-QTDE-OCOR
               MOVE ZEROS               TO GFCTNH-QTDE-TOT-REG
               MOVE GFCTNI-FUNC-BDSCO   TO GFCTNH-FUNC-BDSCO
               MOVE GFCTNI-FIM          TO GFCTNH-FIM
               MOVE GFCTNH-DADOS-RST    TO GFCT3O-DADOS-RST
           END-IF.

           PERFORM 2120-RETORNAR-WEB.

      *----------------------------------------------------------------*
       3500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * TRATAR RETORNO LISTA DE VIGENTES DE FLEXIBILIZACAO             *
      ******************************************************************
      *----------------------------------------------------------------*
       3510-TRATAR-RET-LISTA-VIG-FLEX  SECTION.
      *----------------------------------------------------------------*

           IF  GFCTNI-FIM              EQUAL 'S'
               MOVE WRK-MSGF           TO GFCT0L-TPOMSG-WEB
           ELSE
               IF  IND-1               LESS 3
                   MOVE WRK-MSGI       TO GFCT0L-TPOMSG-WEB
                   MOVE GFCTNI-QTDE-OCOR
                                       TO GFCTNH-QTDE-OCOR
                   MOVE ZEROS          TO GFCTNH-QTDE-TOT-REG
               ELSE
                   MOVE WRK-RSTA       TO GFCT0L-TPOMSG-WEB
               END-IF
           END-IF.

           IF  GFCT0L-TPOMSG-WEB       EQUAL WRK-RSTA OR WRK-MSGF
               MOVE ZEROS              TO GFCT0L-NRO-MSGI
           ELSE
230816         MOVE GFCT0L-NRO-MSGI    TO WRK-NRO-MSGI
230816         ADD 1                   TO WRK-NRO-MSGI
230816         MOVE WRK-NRO-MSGI       TO GFCT0L-NRO-MSGI
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCT0L-TPOMSG-WEB      TO WRK-FUNCAO.

      *----------------------------------------------------------------*
       3510-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * PROCESSAR COMBO AGRUPAMENTOS                                   *
      ******************************************************************
      *----------------------------------------------------------------*
       3600-PROCESSAR-COMBO-AGRUP      SECTION.
      *----------------------------------------------------------------*

           IF  GFCTOK-FIM              EQUAL 'N'
               MOVE SPACES             TO GFCTOL-SAIDA
                                          GFCT0M-AREA-ERROS
               INITIALIZE              GFCT0L-RET-WEB
                                       GFCTOL-SAIDA
                                       GFCT3O-AREA-RESTART
                                       GFCT0M-AREA-ERROS
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCTOK-FUNCAO          TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTOK-ENTRADA
                                             GFCTOL-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-MODULO-MSG
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               PERFORM 1100-FORMATAR-ERRO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  GFCTOL-ERRO             EQUAL 9
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           PERFORM 3610-TRATAR-RET-COMBO-AGRUP.

           MOVE GFCTOL-SAIDA           TO GFCT0L-ENTRADA.

           IF  WRK-FUNCAO               EQUAL WRK-RSTA
               MOVE 255                 TO GFCT3O-LL
               MOVE GFCTOL-FUNCAO       TO GFCT3O-FUNCAO
               MOVE GFCTOL-FILTRO       TO GFCTOK-FILTRO
               MOVE GFCTOL-QTDE-OCOR    TO GFCTOK-QTDE-OCOR
               MOVE ZEROS               TO GFCTOK-QTDE-TOT-REG
               MOVE GFCTOL-PONTEIRO     TO GFCTOK-PONTEIRO
               MOVE GFCTOL-FUNC-BDSCO   TO GFCTOK-FUNC-BDSCO
               MOVE GFCTOL-FIM          TO GFCTOK-FIM
               MOVE GFCTOK-DADOS-RST    TO GFCT3O-DADOS-RST
           END-IF.

           PERFORM 2120-RETORNAR-WEB.

      *----------------------------------------------------------------*
       3600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * TRATAR RETORNO COMBO AGRUPAMENTOS                              *
      ******************************************************************
      *----------------------------------------------------------------*
       3610-TRATAR-RET-COMBO-AGRUP     SECTION.
      *----------------------------------------------------------------*

           IF  GFCTOL-FIM              EQUAL 'S'
               MOVE WRK-MSGF           TO GFCT0L-TPOMSG-WEB
           ELSE
               IF  IND-1               LESS 3
                   MOVE WRK-MSGI       TO GFCT0L-TPOMSG-WEB
                   MOVE GFCTOL-PONTEIRO
                                       TO GFCTOK-PONTEIRO
               ELSE
                   MOVE WRK-RSTA       TO GFCT0L-TPOMSG-WEB
               END-IF
           END-IF.

           IF  GFCT0L-TPOMSG-WEB       EQUAL WRK-RSTA OR WRK-MSGF
               MOVE ZEROS              TO GFCT0L-NRO-MSGI
           ELSE
230816         MOVE GFCT0L-NRO-MSGI    TO WRK-NRO-MSGI
230816         ADD 1                   TO WRK-NRO-MSGI
230816         MOVE WRK-NRO-MSGI       TO GFCT0L-NRO-MSGI
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCT0L-TPOMSG-WEB      TO WRK-FUNCAO.

      *----------------------------------------------------------------*
       3610-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * PROCESSAR COMBO DE SEGMENTOS VIGENTES                          *
      ******************************************************************
      *----------------------------------------------------------------*
       3700-PROCESSAR-COMBO-SEG-VIG    SECTION.
      *----------------------------------------------------------------*

           IF  GFCTG7-FIM              EQUAL 'N'
               MOVE SPACES             TO GFCTG8-SAIDA
                                          GFCT0M-AREA-ERROS
               INITIALIZE              GFCT0L-RET-WEB
                                       GFCTG8-SAIDA
                                       GFCT3O-AREA-RESTART
                                       GFCT0M-AREA-ERROS
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCTG7-FUNCAO          TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTG7-ENTRADA
                                             GFCTG8-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-MODULO-MSG
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               PERFORM 1100-FORMATAR-ERRO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  GFCTG8-ERRO             EQUAL 9
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           PERFORM 3710-TRATAR-RET-COMBO-SEG-VIG.

           MOVE GFCTG8-SAIDA           TO GFCT0L-ENTRADA.

           IF  WRK-FUNCAO               EQUAL WRK-RSTA
               MOVE 255                 TO GFCT3O-LL
               MOVE GFCTG8-FUNCAO       TO GFCT3O-FUNCAO
               MOVE GFCTG8-FILTRO       TO GFCTG7-FILTRO
               MOVE GFCTG8-QTDE-OCOR    TO GFCTG7-QTDE-OCOR
               MOVE ZEROS               TO GFCTG7-QTDE-TOT-REG
               MOVE GFCTG8-PONTEIRO     TO GFCTG7-PONTEIRO
               MOVE GFCTG8-FUNC-BDSCO   TO GFCTG7-FUNC-BDSCO
               MOVE GFCTG8-FIM          TO GFCTG7-FIM
               MOVE GFCTG7-DADOS-RST    TO GFCT3O-DADOS-RST
           END-IF.

           PERFORM 2120-RETORNAR-WEB.

      *----------------------------------------------------------------*
       3700-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * TRATAR RETORNO COMBO DE SEGMENTOS VIGENTES                     *
      ******************************************************************
      *----------------------------------------------------------------*
       3710-TRATAR-RET-COMBO-SEG-VIG   SECTION.
      *----------------------------------------------------------------*

           IF  GFCTG8-FIM              EQUAL 'S'
               MOVE WRK-MSGF           TO GFCT0L-TPOMSG-WEB
           ELSE
               IF  IND-1               LESS 3
                   MOVE WRK-MSGI       TO GFCT0L-TPOMSG-WEB
                   MOVE GFCTG8-PONTEIRO TO GFCTG7-PONTEIRO
               ELSE
                   MOVE WRK-RSTA       TO GFCT0L-TPOMSG-WEB
               END-IF
           END-IF.

           IF  GFCT0L-TPOMSG-WEB       EQUAL WRK-RSTA OR WRK-MSGF
               MOVE ZEROS              TO GFCT0L-NRO-MSGI
           ELSE
230816         MOVE GFCT0L-NRO-MSGI    TO WRK-NRO-MSGI
230816         ADD 1                   TO WRK-NRO-MSGI
230816         MOVE WRK-NRO-MSGI       TO GFCT0L-NRO-MSGI
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCT0L-TPOMSG-WEB      TO WRK-FUNCAO.

      *----------------------------------------------------------------*
       3710-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * PROCESSAR TEXT DE NOME DE POSTO / CORRESPONDENTE BANCARIO      *
      ******************************************************************
      *----------------------------------------------------------------*
       3800-PROCESSAR-TEXT-POSTO-CORR  SECTION.
      *----------------------------------------------------------------*

           IF  GFCTGB-FIM              EQUAL 'N'
               MOVE SPACES             TO GFCTGC-SAIDA
                                          GFCT0M-AREA-ERROS
               INITIALIZE              GFCT0L-RET-WEB
                                       GFCTGC-SAIDA
                                       GFCT0M-AREA-ERROS
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCTGB-FUNCAO          TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTGB-ENTRADA
                                             GFCTGC-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-MODULO-MSG
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               PERFORM 1100-FORMATAR-ERRO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  GFCTGC-ERRO             EQUAL 9
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           PERFORM 2210-TRATAR-RETORNO-COMUM.

           MOVE GFCTGC-SAIDA           TO GFCT0L-ENTRADA.

           PERFORM 2120-RETORNAR-WEB.

      *----------------------------------------------------------------*
       3800-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * PROCESSAR TEXT DE NOME DO MUNICIPIO                            *
      ******************************************************************
      *----------------------------------------------------------------*
       3900-PROCESSAR-TEXT-NOME-MUNIC  SECTION.
      *----------------------------------------------------------------*

           IF  GFCTKJ-FIM              EQUAL 'N'
               MOVE SPACES             TO GFCTKK-SAIDA
                                          GFCT0M-AREA-ERROS
               INITIALIZE              GFCT0L-RET-WEB
                                       GFCTKK-SAIDA
                                       GFCT0M-AREA-ERROS
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCTKJ-FUNCAO          TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTKJ-ENTRADA
                                             GFCTKK-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-MODULO-MSG
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               PERFORM 1100-FORMATAR-ERRO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  GFCTKK-ERRO             EQUAL 9
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           PERFORM 2210-TRATAR-RETORNO-COMUM.

           MOVE GFCTKK-SAIDA           TO GFCT0L-ENTRADA.

           PERFORM 2120-RETORNAR-WEB.

      *----------------------------------------------------------------*
       3900-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * PROCESSAR COMBO DE NOME DE UF                                  *
      ******************************************************************
      *----------------------------------------------------------------*
       4000-PROCESSAR-COMBO-NOME-UF    SECTION.
      *----------------------------------------------------------------*

           IF  GFCTKL-FIM              EQUAL 'N'
               MOVE SPACES             TO GFCTKM-SAIDA
                                          GFCT0M-AREA-ERROS
               INITIALIZE              GFCT0L-RET-WEB
                                       GFCTKM-SAIDA
                                       GFCT3O-AREA-RESTART
                                       GFCT0M-AREA-ERROS
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCTKL-FUNCAO          TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTKL-ENTRADA
                                             GFCTKM-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-MODULO-MSG
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               PERFORM 1100-FORMATAR-ERRO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  GFCTKM-ERRO             EQUAL 9
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           PERFORM 4100-TRATAR-RET-COMBO-NOME-UF.

           MOVE GFCTKM-SAIDA           TO GFCT0L-ENTRADA.

           IF  WRK-FUNCAO               EQUAL WRK-RSTA
               MOVE 255                 TO GFCT3O-LL
               MOVE GFCTKM-FUNCAO       TO GFCT3O-FUNCAO
               MOVE GFCTKM-FILTRO       TO GFCTKL-FILTRO
               MOVE GFCTKM-QTDE-OCOR    TO GFCTKL-QTDE-OCOR
               MOVE ZEROS               TO GFCTKL-QTDE-TOT-REG
               MOVE GFCTKM-PONTEIRO     TO GFCTKL-PONTEIRO
               MOVE GFCTKM-FUNC-BDSCO   TO GFCTKL-FUNC-BDSCO
               MOVE GFCTKM-FIM          TO GFCTKL-FIM
               MOVE GFCTKL-DADOS-RST    TO GFCT3O-DADOS-RST
           END-IF.

           PERFORM 2120-RETORNAR-WEB.

      *----------------------------------------------------------------*
       4000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * TRATAR RETORNO COMBO NOME UF                                   *
      ******************************************************************
      *----------------------------------------------------------------*
       4100-TRATAR-RET-COMBO-NOME-UF   SECTION.
      *----------------------------------------------------------------*

           IF  GFCTKM-FIM              EQUAL 'S'
               MOVE WRK-MSGF           TO GFCT0L-TPOMSG-WEB
           ELSE
               IF  IND-1               LESS 3
                   MOVE WRK-MSGI       TO GFCT0L-TPOMSG-WEB
                   MOVE GFCTKM-QTDE-OCOR
                                       TO GFCTKL-QTDE-OCOR
               ELSE
                   MOVE WRK-RSTA       TO GFCT0L-TPOMSG-WEB
               END-IF
           END-IF.

           IF  GFCT0L-TPOMSG-WEB       EQUAL WRK-RSTA OR WRK-MSGF
               MOVE ZEROS              TO GFCT0L-NRO-MSGI
           ELSE
230816         MOVE GFCT0L-NRO-MSGI    TO WRK-NRO-MSGI
230816         ADD 1                   TO WRK-NRO-MSGI
230816         MOVE WRK-NRO-MSGI       TO GFCT0L-NRO-MSGI
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCT0L-TPOMSG-WEB      TO WRK-FUNCAO.

      *----------------------------------------------------------------*
       4100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * PROCESSAR COMBO DE SEGMENTOS COM FLEX AGRUPAMENTO              *
      ******************************************************************
      *----------------------------------------------------------------*
       4200-PROCESSAR-COMBO-SEG-AGRUP  SECTION.
      *----------------------------------------------------------------*

           IF  GFCTOR-FIM              EQUAL 'N'
               MOVE SPACES             TO GFCTOS-SAIDA
                                          GFCT0M-AREA-ERROS
               INITIALIZE              GFCT0L-RET-WEB
                                       GFCTOS-SAIDA
                                       GFCT3O-AREA-RESTART
                                       GFCT0M-AREA-ERROS
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCTOR-FUNCAO          TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTOR-ENTRADA
                                             GFCTOS-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-MODULO-MSG
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               PERFORM 1100-FORMATAR-ERRO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  GFCTOS-ERRO             EQUAL 9
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           PERFORM 4210-TRATAR-RET-COMBO-SEG-AGP.

           MOVE GFCTOS-SAIDA           TO GFCT0L-ENTRADA.

           IF  WRK-FUNCAO               EQUAL WRK-RSTA
               MOVE 255                 TO GFCT3O-LL
               MOVE GFCTOS-FUNCAO       TO GFCT3O-FUNCAO
               MOVE GFCTOS-FILTRO       TO GFCTOR-FILTRO
               MOVE GFCTOS-QTDE-OCOR    TO GFCTOR-QTDE-OCOR
               MOVE ZEROS               TO GFCTOR-QTDE-TOT-REG
               MOVE GFCTOS-PONTEIRO     TO GFCTOR-PONTEIRO
               MOVE GFCTOS-FUNC-BDSCO   TO GFCTOR-FUNC-BDSCO
               MOVE GFCTOS-FIM          TO GFCTOR-FIM
               MOVE GFCTOR-DADOS-RST    TO GFCT3O-DADOS-RST
           END-IF.

           PERFORM 2120-RETORNAR-WEB.

      *----------------------------------------------------------------*
       4200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * TRATAR RETORNO COMBO DE SEGMENTOS COM FLEX AGRUPAMENTO         *
      ******************************************************************
      *----------------------------------------------------------------*
       4210-TRATAR-RET-COMBO-SEG-AGP   SECTION.
      *----------------------------------------------------------------*

           IF  GFCTOS-FIM              EQUAL 'S'
               MOVE WRK-MSGF           TO GFCT0L-TPOMSG-WEB
           ELSE
               IF  IND-1               LESS 3
                   MOVE WRK-MSGI       TO GFCT0L-TPOMSG-WEB
                   MOVE GFCTOS-PONTEIRO
                                       TO GFCTOR-PONTEIRO
               ELSE
                   MOVE WRK-RSTA       TO GFCT0L-TPOMSG-WEB
               END-IF
           END-IF.

           IF  GFCT0L-TPOMSG-WEB       EQUAL WRK-RSTA OR WRK-MSGF
               MOVE ZEROS              TO GFCT0L-NRO-MSGI
           ELSE
230816         MOVE GFCT0L-NRO-MSGI    TO WRK-NRO-MSGI
230816         ADD 1                   TO WRK-NRO-MSGI
230816         MOVE WRK-NRO-MSGI       TO GFCT0L-NRO-MSGI
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCT0L-TPOMSG-WEB      TO WRK-FUNCAO.

      *----------------------------------------------------------------*
       4210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * PROCESSAR COMBO DE UF COM FLEX DE AGRUPAMENTO                  *
      ******************************************************************
      *----------------------------------------------------------------*
       4300-PROCESSAR-COMBO-UF-AGRUP   SECTION.
      *----------------------------------------------------------------*

           IF  GFCTOT-FIM              EQUAL 'N'
               MOVE SPACES             TO GFCTOU-SAIDA
                                          GFCT0M-AREA-ERROS
               INITIALIZE              GFCT0L-RET-WEB
                                       GFCTOU-SAIDA
                                       GFCT3O-AREA-RESTART
                                       GFCT0M-AREA-ERROS
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCTOT-FUNCAO          TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTOT-ENTRADA
                                             GFCTOU-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-MODULO-MSG
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               PERFORM 1100-FORMATAR-ERRO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  GFCTOU-ERRO             EQUAL 9
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           PERFORM 4310-TRATAR-RET-COMBO-UF-AGRUP.

           MOVE GFCTOU-SAIDA           TO GFCT0L-ENTRADA.

           IF  WRK-FUNCAO               EQUAL WRK-RSTA
               MOVE 255                 TO GFCT3O-LL
               MOVE GFCTOU-FUNCAO       TO GFCT3O-FUNCAO
               MOVE GFCTOU-FILTRO       TO GFCTOT-FILTRO
               MOVE GFCTOU-QTDE-OCOR    TO GFCTOT-QTDE-OCOR
               MOVE ZEROS               TO GFCTOT-QTDE-TOT-REG
               MOVE GFCTOU-PONTEIRO     TO GFCTOT-PONTEIRO
               MOVE GFCTOU-FUNC-BDSCO   TO GFCTOT-FUNC-BDSCO
               MOVE GFCTOU-FIM          TO GFCTOT-FIM
               MOVE GFCTOT-DADOS-RST    TO GFCT3O-DADOS-RST
           END-IF.

           PERFORM 2120-RETORNAR-WEB.

      *----------------------------------------------------------------*
       4300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * TRATAR RETORNO COMBO DE UF COM FLEX DE AGRUPAMENTO             *
      ******************************************************************
      *----------------------------------------------------------------*
       4310-TRATAR-RET-COMBO-UF-AGRUP  SECTION.
      *----------------------------------------------------------------*

           IF  GFCTOU-FIM              EQUAL 'S'
               MOVE WRK-MSGF           TO GFCT0L-TPOMSG-WEB
           ELSE
               IF  IND-1               LESS 3
                   MOVE WRK-MSGI       TO GFCT0L-TPOMSG-WEB
                   MOVE GFCTOU-PONTEIRO
                                       TO GFCTOT-PONTEIRO
               ELSE
                   MOVE WRK-RSTA       TO GFCT0L-TPOMSG-WEB
               END-IF
           END-IF.

           IF  GFCT0L-TPOMSG-WEB       EQUAL WRK-RSTA OR WRK-MSGF
               MOVE ZEROS              TO GFCT0L-NRO-MSGI
           ELSE
230816         MOVE GFCT0L-NRO-MSGI    TO WRK-NRO-MSGI
230816         ADD 1                   TO WRK-NRO-MSGI
230816         MOVE WRK-NRO-MSGI       TO GFCT0L-NRO-MSGI
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCT0L-TPOMSG-WEB      TO WRK-FUNCAO.

      *----------------------------------------------------------------*
       4310-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * PROCESSAR COMBO DE TARIFAS COM FLEX AGRUPAMENTO                *
      ******************************************************************
      *----------------------------------------------------------------*
       4400-PROCESSAR-COMBO-TAR-AGP    SECTION.
      *----------------------------------------------------------------*

           IF  GFCTOO-FIM              EQUAL 'N'
               MOVE SPACES             TO GFCTOP-SAIDA
                                          GFCT0M-AREA-ERROS
               INITIALIZE              GFCT0L-RET-WEB
                                       GFCTOP-SAIDA
                                       GFCT3O-AREA-RESTART
                                       GFCT0M-AREA-ERROS
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCTOO-FUNCAO          TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTOO-ENTRADA
                                             GFCTOP-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-MODULO-MSG
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               PERFORM 1100-FORMATAR-ERRO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  GFCTOP-ERRO             EQUAL 9
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           PERFORM 4410-TRATAR-RET-COMBO-TARIF-AG.

           MOVE GFCTOP-SAIDA           TO GFCT0L-ENTRADA.

           IF  WRK-FUNCAO               EQUAL WRK-RSTA
               MOVE 255                 TO GFCT3O-LL
               MOVE GFCTOP-FUNCAO       TO GFCT3O-FUNCAO
               MOVE GFCTOP-FILTRO       TO GFCTOO-FILTRO
               MOVE GFCTOP-QTDE-OCOR    TO GFCTOO-QTDE-OCOR
               MOVE ZEROS               TO GFCTOO-QTDE-TOT-REG
               MOVE GFCTOP-PONTEIRO     TO GFCTOO-PONTEIRO
               MOVE GFCTOP-FUNC-BDSCO   TO GFCTOO-FUNC-BDSCO
               MOVE GFCTOP-FIM          TO GFCTOO-FIM
               MOVE GFCTOO-DADOS-RST    TO GFCT3O-DADOS-RST
           END-IF.

           PERFORM 2120-RETORNAR-WEB.

      *----------------------------------------------------------------*
       4400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * TRATAR RETORNO COMBO DE TARIFAS COM FLEX DE AGRUPAMENTO        *
      ******************************************************************
      *----------------------------------------------------------------*
       4410-TRATAR-RET-COMBO-TARIF-AG  SECTION.
      *----------------------------------------------------------------*

           IF  GFCTOP-FIM              EQUAL 'S'
               MOVE WRK-MSGF           TO GFCT0L-TPOMSG-WEB
           ELSE
               IF  IND-1               LESS 3
                   MOVE WRK-MSGI       TO GFCT0L-TPOMSG-WEB
                   MOVE GFCTOP-PONTEIRO
                                       TO GFCTOO-PONTEIRO
               ELSE
                   MOVE WRK-RSTA       TO GFCT0L-TPOMSG-WEB
               END-IF
           END-IF.

           IF  GFCT0L-TPOMSG-WEB       EQUAL WRK-RSTA OR WRK-MSGF
               MOVE ZEROS              TO GFCT0L-NRO-MSGI
           ELSE
230816         MOVE GFCT0L-NRO-MSGI    TO WRK-NRO-MSGI
230816         ADD 1                   TO WRK-NRO-MSGI
230816         MOVE WRK-NRO-MSGI       TO GFCT0L-NRO-MSGI
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCT0L-TPOMSG-WEB      TO WRK-FUNCAO.

      *----------------------------------------------------------------*
       4410-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * PROCESSAR TEXT NOME AGENCIA COM FLEXIBILIZACAO                 *
      ******************************************************************
      *----------------------------------------------------------------*
       4500-PROCESSAR-TEXT-NOME-AGE    SECTION.
      *----------------------------------------------------------------*

           IF  GFCTOY-FIM              EQUAL 'N'
               MOVE SPACES             TO GFCTOZ-SAIDA
                                          GFCT0M-AREA-ERROS
               INITIALIZE              GFCT0L-RET-WEB
                                       GFCTOZ-SAIDA
                                       GFCT0M-AREA-ERROS
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCTOY-FUNCAO          TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTOY-ENTRADA
                                             GFCTOZ-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-MODULO-MSG
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               PERFORM 1100-FORMATAR-ERRO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  GFCTOZ-ERRO             EQUAL 9
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           PERFORM 2210-TRATAR-RETORNO-COMUM.

           MOVE GFCTOZ-SAIDA           TO GFCT0L-ENTRADA.

           PERFORM 2120-RETORNAR-WEB.

      *----------------------------------------------------------------*
       4500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * PROCESSAR TEXT DE NOME DE POSTO BANCARIO COM FLEXIBILIZACAO    *
      ******************************************************************
      *----------------------------------------------------------------*
       4600-PROCESSAR-TEXT-POSTO-FLEX  SECTION.
      *----------------------------------------------------------------*

           IF  GFCTP0-FIM              EQUAL 'N'
               MOVE SPACES             TO GFCTP1-SAIDA
                                          GFCT0M-AREA-ERROS
               INITIALIZE              GFCT0L-RET-WEB
                                       GFCTP1-SAIDA
                                       GFCT0M-AREA-ERROS
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCTP0-FUNCAO          TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTP0-ENTRADA
                                             GFCTP1-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-MODULO-MSG
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               PERFORM 1100-FORMATAR-ERRO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  GFCTP1-ERRO             EQUAL 9
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           PERFORM 2210-TRATAR-RETORNO-COMUM.

           MOVE GFCTP1-SAIDA           TO GFCT0L-ENTRADA.

           PERFORM 2120-RETORNAR-WEB.

      *----------------------------------------------------------------*
       4600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * PROCESSAR TEXT DE NOME DE CORRESPONDENTE BANCARIO COM FLEX     *
      ******************************************************************
      *----------------------------------------------------------------*
       4700-PROCESSAR-TEXT-CORRESP     SECTION.
      *----------------------------------------------------------------*

           IF  GFCTP2-FIM              EQUAL 'N'
               MOVE SPACES             TO GFCTP3-SAIDA
                                          GFCT0M-AREA-ERROS
               INITIALIZE              GFCT0L-RET-WEB
                                       GFCTP3-SAIDA
                                       GFCT0M-AREA-ERROS
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCTP2-FUNCAO          TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTP2-ENTRADA
                                             GFCTP3-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-MODULO-MSG
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               PERFORM 1100-FORMATAR-ERRO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  GFCTP3-ERRO             EQUAL 9
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           PERFORM 2210-TRATAR-RETORNO-COMUM.

           MOVE GFCTP3-SAIDA           TO GFCT0L-ENTRADA.

           PERFORM 2120-RETORNAR-WEB.

      *----------------------------------------------------------------*
       4700-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * PROCESSAR TEXT DE NOME DE MUNICIPIO COM FLEXIBILIZACAO         *
      ******************************************************************
      *----------------------------------------------------------------*
       4800-PROCESSAR-TEXT-MUN-FLEX    SECTION.
      *----------------------------------------------------------------*

           IF  GFCTP4-FIM              EQUAL 'N'
               MOVE SPACES             TO GFCTP5-SAIDA
                                          GFCT0M-AREA-ERROS
               INITIALIZE              GFCT0L-RET-WEB
                                       GFCTP5-SAIDA
                                       GFCT0M-AREA-ERROS
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCTP4-FUNCAO          TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTP4-ENTRADA
                                             GFCTP5-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-MODULO-MSG
               MOVE WRK-MSG05          TO GFCT0M-TEXTO
               PERFORM 1100-FORMATAR-ERRO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  GFCTP5-ERRO             EQUAL 9
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           PERFORM 2210-TRATAR-RETORNO-COMUM.

           MOVE GFCTP5-SAIDA           TO GFCT0L-ENTRADA.

           PERFORM 2120-RETORNAR-WEB.

      *----------------------------------------------------------------*
       4800-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * PROCESSAR COMBO DE CONJUNTO DE SERVICO                         *
      ******************************************************************
      *----------------------------------------------------------------*
       4900-PROCESSAR-COMBO-CJTO-SERV  SECTION.
      *----------------------------------------------------------------*

           IF  WAM0-FIM              EQUAL 'N'
               MOVE SPACES             TO WAM1-SAIDA
                                          GFCT0M-AREA-ERROS

               INITIALIZE              GFCT0L-RET-WEB
                                       WAM1-SAIDA
                                       GFCT3O-AREA-RESTART
                                       GFCT0M-AREA-ERROS
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE WAM0-FUNCAO          TO WRK-MODULO.

           CALL WRK-MODULO          USING WAM0-ENTRADA
                                          WAM1-SAIDA
                                          GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-MODULO-MSG
               MOVE WRK-MSG05          TO GFCT0M-TEXTO

               PERFORM 1100-FORMATAR-ERRO

               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  WAM1-ERRO            EQUAL 9
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           PERFORM 4910-TRATAR-RET-COMBO-CJTO-SER.

           MOVE WAM1-SAIDA           TO GFCT0L-ENTRADA.

           IF  WRK-FUNCAO               EQUAL WRK-RSTA
               MOVE 255               TO GFCT3O-LL
               MOVE WAM1-FUNCAO       TO GFCT3O-FUNCAO
               MOVE WAM1-FILTRO       TO WAM0-FILTRO
               MOVE WAM1-QTDE-OCOR    TO WAM0-QTDE-OCOR
               MOVE ZEROS             TO WAM0-QTDE-TOT-REG
               MOVE WAM1-FUNC-BDSCO   TO WAM0-FUNC-BDSCO
               MOVE WAM1-PONTEIRO     TO WAM0-PONTEIRO
               MOVE WAM1-FIM          TO WAM0-FIM
               MOVE WAM1-TIPO-VIGENCIA
                                      TO WAM0-TIPO-VIGENCIA
               MOVE WAM1-TIPO-DATA    TO WAM0-TIPO-DATA
               MOVE WAM0-DADOS-RST    TO GFCT3O-DADOS-RST
           END-IF.

           PERFORM 2120-RETORNAR-WEB.

      *----------------------------------------------------------------*
       4900-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * TRATAR RETORNO COMBO DE CONJUTO DE SERVICO                     *
      ******************************************************************
      *----------------------------------------------------------------*
       4910-TRATAR-RET-COMBO-CJTO-SER  SECTION.
      *----------------------------------------------------------------*

           IF  WAM1-FIM              EQUAL 'S'
               MOVE WRK-MSGF           TO GFCT0L-TPOMSG-WEB
           ELSE
               IF  IND-1               LESS 3
                   MOVE WRK-MSGI       TO GFCT0L-TPOMSG-WEB
                   MOVE WAM1-FILTRO    TO WAM0-FILTRO
                   MOVE WAM1-PONTEIRO
                                       TO WAM0-PONTEIRO
               ELSE
                   MOVE WRK-RSTA       TO GFCT0L-TPOMSG-WEB
               END-IF
           END-IF.

           IF  GFCT0L-TPOMSG-WEB       EQUAL WRK-RSTA OR WRK-MSGF
               MOVE ZEROS              TO GFCT0L-NRO-MSGI
           ELSE
230816         MOVE GFCT0L-NRO-MSGI    TO WRK-NRO-MSGI
230816         ADD 1                   TO WRK-NRO-MSGI
230816         MOVE WRK-NRO-MSGI       TO GFCT0L-NRO-MSGI
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCT0L-TPOMSG-WEB      TO WRK-FUNCAO.

      *----------------------------------------------------------------*
       4910-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * PROCESSAR COMBO TARIFA                                         *
      ******************************************************************
      *----------------------------------------------------------------*
       5000-PROCESSAR-COMBO-TARIFA     SECTION.
      *----------------------------------------------------------------*

           IF  GFCTVR-FIM              EQUAL 'N'
               MOVE SPACES             TO GFCTVS-SAIDA
                                          GFCT0M-AREA-ERROS

               INITIALIZE              GFCT0L-RET-WEB
                                       GFCTVS-SAIDA
                                       GFCT3O-AREA-RESTART
                                       GFCT0M-AREA-ERROS
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCTVR-FUNCAO          TO WRK-MODULO.

           CALL 'POOL0081'             USING WRK-MODULO
                                             GFCTVR-ENTRADA
                                             GFCTVS-SAIDA
                                             GFCT0M-AREA-ERROS.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-MODULO         TO WRK-MODULO-MSG
               MOVE WRK-MSG05          TO GFCT0M-TEXTO

               PERFORM 1100-FORMATAR-ERRO

               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  GFCTVS-ERRO             EQUAL 9
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           PERFORM 5010-TRATAR-RET-COMBO-TARIFAS.

           MOVE GFCTVS-SAIDA           TO GFCT0L-ENTRADA.

           IF  WRK-FUNCAO               EQUAL WRK-RSTA
               MOVE 255                 TO GFCT3O-LL
               MOVE GFCTVS-FUNCAO       TO GFCT3O-FUNCAO
               MOVE GFCTVS-FILTRO       TO GFCTVR-FILTRO
               MOVE GFCTVS-QTDE-OCOR    TO GFCTVR-QTDE-OCOR
               MOVE ZEROS               TO GFCTVR-QTDE-TOT-REG
               MOVE GFCTVS-FUNC-BDSCO   TO GFCTVR-FUNC-BDSCO
               MOVE GFCTVS-PONTEIRO     TO GFCTVR-PONTEIRO
               MOVE GFCTVS-FIM          TO GFCTVR-FIM
               MOVE GFCTVS-TIPO-VIGENCIA
                                        TO GFCTVR-TIPO-VIGENCIA
               MOVE GFCTVS-TIPO-DATA    TO GFCTVR-TIPO-DATA
               MOVE GFCTVR-DADOS-RST    TO GFCT3O-DADOS-RST
           END-IF.

           PERFORM 2120-RETORNAR-WEB.

      *----------------------------------------------------------------*
       5000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * TRATAR RETORNO COMBO TARIFAS                                   *
      ******************************************************************
      *----------------------------------------------------------------*
       5010-TRATAR-RET-COMBO-TARIFAS   SECTION.
      *----------------------------------------------------------------*

           IF  GFCTVS-FIM              EQUAL 'S'
               MOVE WRK-MSGF           TO GFCT0L-TPOMSG-WEB
           ELSE
               IF  IND-1               LESS 3
                   MOVE WRK-MSGI       TO GFCT0L-TPOMSG-WEB
                   MOVE GFCTVS-FILTRO  TO GFCTVR-FILTRO
                   MOVE GFCTVS-PONTEIRO
                                       TO GFCTVR-PONTEIRO
               ELSE
                   MOVE WRK-RSTA       TO GFCT0L-TPOMSG-WEB
               END-IF
           END-IF.

           IF  GFCT0L-TPOMSG-WEB       EQUAL WRK-RSTA OR WRK-MSGF
               MOVE ZEROS              TO GFCT0L-NRO-MSGI
           ELSE
230816         MOVE GFCT0L-NRO-MSGI    TO WRK-NRO-MSGI
230816         ADD 1                   TO WRK-NRO-MSGI
230816         MOVE WRK-NRO-MSGI       TO GFCT0L-NRO-MSGI
           END-IF.

           MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
           MOVE GFCT0L-TPOMSG-WEB      TO WRK-FUNCAO.

      *----------------------------------------------------------------*
       5010-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

080708******************************************************************
080708* PROCESSAR COMBO TARIFA VERIFCANDO PERMISSAO                    *
080708******************************************************************
080708*----------------------------------------------------------------*
080708 5500-PROCESSAR-COMBO-TAR5588    SECTION.
080708*----------------------------------------------------------------*
080708
080708     IF  WAAM-FIM                EQUAL 'N'
080708         MOVE SPACES             TO WAAN-SAIDA
080708                                    GFCT0M-AREA-ERROS
080708
080708         INITIALIZE              GFCT0L-RET-WEB
080708                                 WAAN-SAIDA
080708                                 GFCT3O-AREA-RESTART
080708                                 GFCT0M-AREA-ERROS
080708     END-IF.
080708
080708     MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
080708     MOVE WAAM-FUNCAO            TO WRK-MODULO.
080708
080708     CALL 'POOL0081'             USING WRK-MODULO
080708                                       WAAM-ENTRADA
080708                                       WAAN-SAIDA
080708                                       GFCT0M-AREA-ERROS.
080708
080708     IF  RETURN-CODE             NOT EQUAL ZEROS
080708         MOVE WRK-MODULO         TO WRK-MODULO-MSG
080708         MOVE WRK-MSG05          TO GFCT0M-TEXTO
080708
080708         PERFORM 1100-FORMATAR-ERRO
080708
080708         PERFORM 9999-PROCESSAR-ROTINA-ERRO
080708     END-IF.
080708
080708     IF  WAAN-ERRO                EQUAL 9
080708         PERFORM 9999-PROCESSAR-ROTINA-ERRO
080708     END-IF.
080708
080708     PERFORM 5510-TRATAR-RET-COMBO-TAR5588.
080708
080708     MOVE WAAN-SAIDA              TO GFCT0L-ENTRADA.
080708
080708     IF  WRK-FUNCAO               EQUAL WRK-RSTA
080708         MOVE 255                 TO GFCT3O-LL
080708         MOVE WAAN-FUNCAO         TO GFCT3O-FUNCAO
080708         MOVE WAAN-FILTRO         TO WAAM-FILTRO
080708         MOVE WAAN-QTDE-OCOR      TO WAAM-QTDE-OCOR
080708         MOVE ZEROS               TO WAAM-QTDE-TOT-REG
080708         MOVE WAAN-FUNC-BDSCO     TO WAAM-FUNC-BDSCO
080708         MOVE WAAN-PONTEIRO       TO WAAM-PONTEIRO
080708         MOVE WAAN-FIM            TO WAAM-FIM
080708         MOVE WAAN-TIPO-VIGENCIA
080708                                  TO WAAM-TIPO-VIGENCIA
080708         MOVE WAAN-TIPO-DATA      TO WAAM-TIPO-DATA
080708         MOVE WAAM-DADOS-RST      TO GFCT3O-DADOS-RST
080708     END-IF.
080708
080708     PERFORM 2120-RETORNAR-WEB.
080708
080708*----------------------------------------------------------------*
080708 5500-99-FIM.                    EXIT.
080708*----------------------------------------------------------------*
080708
080708******************************************************************
080708* TRATAR RETORNO COMBO TARIFAS QUE VERIFICA PERMISSAO            *
080708******************************************************************
080708*----------------------------------------------------------------*
080708 5510-TRATAR-RET-COMBO-TAR5588   SECTION.
080708*----------------------------------------------------------------*
080708
080708     IF  WAAN-FIM                EQUAL 'S'
080708         MOVE WRK-MSGF           TO GFCT0L-TPOMSG-WEB
080708     ELSE
080708         IF  IND-1               LESS 3
080708             MOVE WRK-MSGI       TO GFCT0L-TPOMSG-WEB
080708             MOVE WAAN-FILTRO    TO WAAM-FILTRO
080708             MOVE WAAN-PONTEIRO
080708                                 TO WAAM-PONTEIRO
080708         ELSE
080708             MOVE WRK-RSTA       TO GFCT0L-TPOMSG-WEB
080708         END-IF
080708     END-IF.
080708
080708     IF  GFCT0L-TPOMSG-WEB       EQUAL WRK-RSTA OR WRK-MSGF
080708         MOVE ZEROS              TO GFCT0L-NRO-MSGI
080708     ELSE
230816         MOVE GFCT0L-NRO-MSGI    TO WRK-NRO-MSGI
230816         ADD 1                   TO WRK-NRO-MSGI
230816         MOVE WRK-NRO-MSGI       TO GFCT0L-NRO-MSGI
080708     END-IF.
080708
080708     MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
080708     MOVE GFCT0L-TPOMSG-WEB      TO WRK-FUNCAO.
080708
080708*----------------------------------------------------------------*
080708 5510-99-FIM.                    EXIT.
080708*----------------------------------------------------------------*

2912  ******************************************************************
2912  * PROCESSAR COMBO DE CONJUNTO DE SERVICOS HISTORICO              *
2912  ******************************************************************
2912  *----------------------------------------------------------------*
2912   5600-PROCESSA-COMBO-GFCT5169    SECTION.
2912  *----------------------------------------------------------------*
2912
2912       IF  WAJQ-FIM                EQUAL 'N'
2912           MOVE SPACES             TO WAJR-SAIDA
2912                                      GFCT0M-AREA-ERROS
2912
2912           INITIALIZE                 GFCT0L-RET-WEB
2912                                      WAJR-SAIDA
2912                                      GFCT3O-AREA-RESTART
2912                                      GFCT0M-AREA-ERROS
2912       END-IF.
2912
2912       MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
2912       MOVE WAJQ-FUNCAO            TO WRK-MODULO.
2912
2912       CALL WRK-MODULO          USING WAJQ-ENTRADA
2912                                      WAJR-SAIDA
2912                                      GFCT0M-AREA-ERROS.
2912
2912       IF  RETURN-CODE             NOT EQUAL ZEROS
2912           MOVE WRK-MODULO         TO WRK-MODULO-MSG
2912           MOVE WRK-MSG05          TO GFCT0M-TEXTO
2912
2912           PERFORM 1100-FORMATAR-ERRO
2912
2912           PERFORM 9999-PROCESSAR-ROTINA-ERRO
2912       END-IF.
2912
2912       IF  WAJR-ERRO                EQUAL 9
2912           PERFORM 9999-PROCESSAR-ROTINA-ERRO
2912       END-IF.
2912
2912       PERFORM 5610-TRATA-RET-COMBO-GFCT5169
2912
2912       MOVE WAJR-SAIDA              TO GFCT0L-ENTRADA.
2912
2912       IF  WRK-FUNCAO               EQUAL WRK-RSTA
2912           MOVE 255                 TO GFCT3O-LL
2912           MOVE WAJR-FUNCAO         TO GFCT3O-FUNCAO
2912           MOVE WAJR-FILTRO         TO WAJQ-FILTRO
2912           MOVE WAJR-QTDE-OCOR      TO WAJQ-QTDE-OCOR
2912           MOVE ZEROS               TO WAJQ-QTDE-TOT-REG
2912           MOVE WAJR-FUNC-BDSCO     TO WAJQ-FUNC-BDSCO
2912           MOVE WAJR-PONTEIRO       TO WAJQ-PONTEIRO
2912           MOVE WAJR-FIM            TO WAJQ-FIM
2912           MOVE WAJQ-DADOS-RST      TO GFCT3O-DADOS-RST
2912       END-IF.
2912
2912       PERFORM 2120-RETORNAR-WEB.
2912
2912  *----------------------------------------------------------------*
2912   5600-99-FIM.                    EXIT.
2912  *----------------------------------------------------------------*
2912
2912  ******************************************************************
2912  * TRATAR RETORNO COMBO DE CONJUNTO DE SERVICOS HIST.             *
2912  ******************************************************************
2912  *----------------------------------------------------------------*
2912   5610-TRATA-RET-COMBO-GFCT5169   SECTION.
2912  *----------------------------------------------------------------*
2912
2912       IF  WAJR-FIM                EQUAL 'S'
2912           MOVE WRK-MSGF           TO GFCT0L-TPOMSG-WEB
2912       ELSE
2912           IF  IND-1               LESS 3
2912               MOVE WRK-MSGI       TO GFCT0L-TPOMSG-WEB
2912               MOVE WAJR-FILTRO    TO WAJQ-FILTRO
2912               MOVE WAJR-PONTEIRO
2912                                   TO WAJQ-PONTEIRO
2912           ELSE
2912               MOVE WRK-RSTA       TO GFCT0L-TPOMSG-WEB
2912           END-IF
2912       END-IF.
2912
2912       IF  GFCT0L-TPOMSG-WEB       EQUAL WRK-RSTA OR WRK-MSGF
2912           MOVE ZEROS              TO GFCT0L-NRO-MSGI
2912       ELSE
2912           MOVE GFCT0L-NRO-MSGI    TO WRK-NRO-MSGI
2912           ADD 1                   TO WRK-NRO-MSGI
2912           MOVE WRK-NRO-MSGI       TO GFCT0L-NRO-MSGI
2912       END-IF.
2912
2912       MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
2912       MOVE GFCT0L-TPOMSG-WEB      TO WRK-FUNCAO.
2912
2912  *----------------------------------------------------------------*
2912   5610-99-FIM.                    EXIT.
2912  *----------------------------------------------------------------*

2912  ******************************************************************
2912  * PROCESSAR COMBO DE SEGMENTO HISTORICO                          *
2912  ******************************************************************
2912  *----------------------------------------------------------------*
2912   5700-PROCESSA-COMBO-GFCT5179    SECTION.
2912  *----------------------------------------------------------------*
2912
2912       IF  WAJU-FIM                EQUAL 'N'
2912           MOVE SPACES             TO WAJV-SAIDA
2912                                      GFCT0M-AREA-ERROS
2912
2912           INITIALIZE                 GFCT0L-RET-WEB
2912                                      WAJV-SAIDA
2912                                      GFCT3O-AREA-RESTART
2912                                      GFCT0M-AREA-ERROS
2912       END-IF.
2912
2912       MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
2912       MOVE WAJU-FUNCAO            TO WRK-MODULO.
2912
2912       CALL WRK-MODULO          USING WAJU-ENTRADA
2912                                      WAJV-SAIDA
2912                                      GFCT0M-AREA-ERROS.
2912
2912       IF  RETURN-CODE             NOT EQUAL ZEROS
2912           MOVE WRK-MODULO         TO WRK-MODULO-MSG
2912           MOVE WRK-MSG05          TO GFCT0M-TEXTO
2912
2912           PERFORM 1100-FORMATAR-ERRO
2912
2912           PERFORM 9999-PROCESSAR-ROTINA-ERRO
2912       END-IF.
2912
2912       IF  WAJV-ERRO                EQUAL 9
2912           PERFORM 9999-PROCESSAR-ROTINA-ERRO
2912       END-IF.
2912
2912       PERFORM 5710-TRATA-RET-COMBO-GFCT5179
2912
2912       MOVE WAJV-SAIDA              TO GFCT0L-ENTRADA.
2912
2912       IF  WRK-FUNCAO               EQUAL WRK-RSTA
2912           MOVE 255                 TO GFCT3O-LL
2912           MOVE WAJV-FUNCAO         TO GFCT3O-FUNCAO
2912           MOVE WAJV-FILTRO         TO WAJU-FILTRO
2912           MOVE WAJV-QTDE-OCOR      TO WAJU-QTDE-OCOR
2912           MOVE ZEROS               TO WAJU-QTDE-TOT-REG
2912           MOVE WAJV-FUNC-BDSCO     TO WAJU-FUNC-BDSCO
2912           MOVE WAJV-PONTEIRO       TO WAJU-PONTEIRO
2912           MOVE WAJV-FIM            TO WAJU-FIM
2912           MOVE WAJU-DADOS-RST      TO GFCT3O-DADOS-RST
2912       END-IF.
2912
2912       PERFORM 2120-RETORNAR-WEB.
2912
2912  *----------------------------------------------------------------*
2912   5700-99-FIM.                    EXIT.
2912  *----------------------------------------------------------------*
2912
2912  ******************************************************************
2912  * TRATAR RETORNO COMBO DE SEGMENTO HISTORICO                     *
2912  ******************************************************************
2912  *----------------------------------------------------------------*
2912   5710-TRATA-RET-COMBO-GFCT5179   SECTION.
2912  *----------------------------------------------------------------*
2912
2912       IF  WAJV-FIM                EQUAL 'S'
2912           MOVE WRK-MSGF           TO GFCT0L-TPOMSG-WEB
2912       ELSE
2912           IF  IND-1               LESS 3
2912               MOVE WRK-MSGI       TO GFCT0L-TPOMSG-WEB
2912               MOVE WAJV-FILTRO    TO WAJU-FILTRO
2912               MOVE WAJV-PONTEIRO  TO WAJU-PONTEIRO
2912           ELSE
2912               MOVE WRK-RSTA       TO GFCT0L-TPOMSG-WEB
2912           END-IF
2912       END-IF.
2912
2912       IF  GFCT0L-TPOMSG-WEB       EQUAL WRK-RSTA OR WRK-MSGF
2912           MOVE ZEROS              TO GFCT0L-NRO-MSGI
2912       ELSE
2912           MOVE GFCT0L-NRO-MSGI    TO WRK-NRO-MSGI
2912           ADD 1                   TO WRK-NRO-MSGI
2912           MOVE WRK-NRO-MSGI       TO GFCT0L-NRO-MSGI
2912       END-IF.
2912
2912       MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
2912       MOVE GFCT0L-TPOMSG-WEB      TO WRK-FUNCAO.
2912
2912  *----------------------------------------------------------------*
2912   5710-99-FIM.                    EXIT.
2912  *----------------------------------------------------------------*

2912  ******************************************************************
2912  * PROCESSAR COMBO DE UF'S HISTORICO                              *
2912  ******************************************************************
2912  *----------------------------------------------------------------*
2912   5800-PROCESSA-COMBO-GFCT5207    SECTION.
2912  *----------------------------------------------------------------*
2912
2912       IF  WAJY-FIM                EQUAL 'N'
2912           MOVE SPACES             TO WAJZ-SAIDA
2912                                      GFCT0M-AREA-ERROS
2912
2912           INITIALIZE                 GFCT0L-RET-WEB
2912                                      WAJZ-SAIDA
2912                                      GFCT3O-AREA-RESTART
2912                                      GFCT0M-AREA-ERROS
2912       END-IF.
2912
2912       MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
2912       MOVE WAJY-FUNCAO            TO WRK-MODULO.
2912
2912       CALL WRK-MODULO          USING WAJY-ENTRADA
2912                                      WAJZ-SAIDA
2912                                      GFCT0M-AREA-ERROS.
2912
2912       IF  RETURN-CODE             NOT EQUAL ZEROS
2912           MOVE WRK-MODULO         TO WRK-MODULO-MSG
2912           MOVE WRK-MSG05          TO GFCT0M-TEXTO
2912
2912           PERFORM 1100-FORMATAR-ERRO
2912
2912           PERFORM 9999-PROCESSAR-ROTINA-ERRO
2912       END-IF.
2912
2912       IF  WAJZ-ERRO                EQUAL 9
2912           PERFORM 9999-PROCESSAR-ROTINA-ERRO
2912       END-IF.
2912
2912       PERFORM 5810-TRATA-RET-COMBO-GFCT5207
2912
2912       MOVE WAJZ-SAIDA              TO GFCT0L-ENTRADA.
2912
2912       IF  WRK-FUNCAO               EQUAL WRK-RSTA
2912           MOVE 255                 TO GFCT3O-LL
2912           MOVE WAJZ-FUNCAO         TO GFCT3O-FUNCAO
2912           MOVE WAJZ-FILTRO         TO WAJY-FILTRO
2912           MOVE WAJZ-QTDE-OCOR      TO WAJY-QTDE-OCOR
2912           MOVE ZEROS               TO WAJY-QTDE-TOT-REG
2912           MOVE WAJZ-FUNC-BDSCO     TO WAJY-FUNC-BDSCO
2912           MOVE WAJZ-PONTEIRO       TO WAJY-PONTEIRO
2912           MOVE WAJZ-FIM            TO WAJY-FIM
2912           MOVE WAJY-DADOS-RST      TO GFCT3O-DADOS-RST
2912       END-IF.
2912
2912       PERFORM 2120-RETORNAR-WEB.
2912
2912  *----------------------------------------------------------------*
2912   5800-99-FIM.                    EXIT.
2912  *----------------------------------------------------------------*
2912
2912  ******************************************************************
2912  * TRATAR RETORNO COMBO DE UF'S HISTORICO                         *
2912  ******************************************************************
2912  *----------------------------------------------------------------*
2912   5810-TRATA-RET-COMBO-GFCT5207   SECTION.
2912  *----------------------------------------------------------------*
2912
2912       IF  WAJZ-FIM                EQUAL 'S'
2912           MOVE WRK-MSGF           TO GFCT0L-TPOMSG-WEB
2912       ELSE
2912           IF  IND-1               LESS 3
2912               MOVE WRK-MSGI       TO GFCT0L-TPOMSG-WEB
2912               MOVE WAJZ-FILTRO    TO WAJY-FILTRO
2912               MOVE WAJZ-PONTEIRO  TO WAJY-PONTEIRO
2912           ELSE
2912               MOVE WRK-RSTA       TO GFCT0L-TPOMSG-WEB
2912           END-IF
2912       END-IF.
2912
2912       IF  GFCT0L-TPOMSG-WEB       EQUAL WRK-RSTA OR WRK-MSGF
2912           MOVE ZEROS              TO GFCT0L-NRO-MSGI
2912       ELSE
2912           MOVE GFCT0L-NRO-MSGI    TO WRK-NRO-MSGI
2912           ADD 1                   TO WRK-NRO-MSGI
2912           MOVE WRK-NRO-MSGI       TO GFCT0L-NRO-MSGI
2912       END-IF.
2912
2912       MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
2912       MOVE GFCT0L-TPOMSG-WEB      TO WRK-FUNCAO.
2912
2912  *----------------------------------------------------------------*
2912   5810-99-FIM.                    EXIT.
2912  *----------------------------------------------------------------*

2912  ******************************************************************
2912  * PROCESSAR COMBO DE CONJUNTO DE SERVICOS VIGENCIA               *
2912  ******************************************************************
2912  *----------------------------------------------------------------*
2912   5900-PROCESSA-COMBO-GFCT5166    SECTION.
2912  *----------------------------------------------------------------*
2912
2912       IF  WAJO-FIM                EQUAL 'N'
2912           MOVE SPACES             TO WAJP-SAIDA
2912                                      GFCT0M-AREA-ERROS
2912
2912           INITIALIZE                 GFCT0L-RET-WEB
2912                                      WAJP-SAIDA
2912                                      GFCT3O-AREA-RESTART
2912                                      GFCT0M-AREA-ERROS
2912       END-IF.
2912
2912       MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
2912       MOVE WAJO-FUNCAO            TO WRK-MODULO.
2912
2912       CALL WRK-MODULO          USING WAJO-ENTRADA
2912                                      WAJP-SAIDA
2912                                      GFCT0M-AREA-ERROS.
2912
2912       IF  RETURN-CODE             NOT EQUAL ZEROS
2912           MOVE WRK-MODULO         TO WRK-MODULO-MSG
2912           MOVE WRK-MSG05          TO GFCT0M-TEXTO
2912
2912           PERFORM 1100-FORMATAR-ERRO
2912
2912           PERFORM 9999-PROCESSAR-ROTINA-ERRO
2912       END-IF.
2912
2912       IF  WAJP-ERRO                EQUAL 9
2912           PERFORM 9999-PROCESSAR-ROTINA-ERRO
2912       END-IF.
2912
2912       PERFORM 5910-TRATA-RET-COMBO-GFCT5166
2912
2912       MOVE WAJP-SAIDA              TO GFCT0L-ENTRADA.
2912
2912       IF  WRK-FUNCAO               EQUAL WRK-RSTA
2912           MOVE 255                 TO GFCT3O-LL
2912           MOVE WAJP-FUNCAO         TO GFCT3O-FUNCAO
2912           MOVE WAJP-FILTRO         TO WAJO-FILTRO
2912           MOVE WAJP-QTDE-OCOR      TO WAJO-QTDE-OCOR
2912           MOVE ZEROS               TO WAJO-QTDE-TOT-REG
2912           MOVE WAJP-FUNC-BDSCO     TO WAJO-FUNC-BDSCO
2912           MOVE WAJP-PONTEIRO       TO WAJO-PONTEIRO
2912           MOVE WAJP-FIM            TO WAJO-FIM
2912           MOVE WAJO-DADOS-RST      TO GFCT3O-DADOS-RST
2912       END-IF.
2912
2912       PERFORM 2120-RETORNAR-WEB.
2912
2912  *----------------------------------------------------------------*
2912   5900-99-FIM.                    EXIT.
2912  *----------------------------------------------------------------*
2912
2912  ******************************************************************
2912  * TRATAR RETORNO COMBO DE CONJUNTO DE SERVICOS VIG.              *
2912  ******************************************************************
2912  *----------------------------------------------------------------*
2912   5910-TRATA-RET-COMBO-GFCT5166   SECTION.
2912  *----------------------------------------------------------------*
2912
TESTE      IF  WAJP-FIM                EQUAL 'S'
2912           MOVE WRK-MSGF           TO GFCT0L-TPOMSG-WEB
2912       ELSE
2912           IF  IND-1               LESS 3
2912               MOVE WRK-MSGI       TO GFCT0L-TPOMSG-WEB
2912               MOVE WAJP-FILTRO    TO WAJO-FILTRO
2912               MOVE WAJP-PONTEIRO
2912                                   TO WAJO-PONTEIRO
2912           ELSE
2912               MOVE WRK-RSTA       TO GFCT0L-TPOMSG-WEB
2912           END-IF
2912       END-IF.
2912
2912       IF  GFCT0L-TPOMSG-WEB       EQUAL WRK-RSTA OR WRK-MSGF
2912           MOVE ZEROS              TO GFCT0L-NRO-MSGI
2912       ELSE
2912           MOVE GFCT0L-NRO-MSGI    TO WRK-NRO-MSGI
2912           ADD 1                   TO WRK-NRO-MSGI
2912           MOVE WRK-NRO-MSGI       TO GFCT0L-NRO-MSGI
2912       END-IF.
2912
2912       MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
2912       MOVE GFCT0L-TPOMSG-WEB      TO WRK-FUNCAO.
2912
2912  *----------------------------------------------------------------*
2912   5910-99-FIM.                    EXIT.
2912  *----------------------------------------------------------------*

2912  ******************************************************************
2912  * PROCESSAR COMBO DE SEGMENTO                                    *
2912  ******************************************************************
2912  *----------------------------------------------------------------*
2912   6100-PROCESSA-COMBO-GFCT5173    SECTION.
2912  *----------------------------------------------------------------*
2912
2912       IF  WAJS-FIM                EQUAL 'N'
2912           MOVE SPACES             TO WAJT-SAIDA
2912                                      GFCT0M-AREA-ERROS
2912
2912           INITIALIZE                 GFCT0L-RET-WEB
2912                                      WAJT-SAIDA
2912                                      GFCT3O-AREA-RESTART
2912                                      GFCT0M-AREA-ERROS
2912       END-IF.
2912
2912       MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
2912       MOVE WAJS-FUNCAO            TO WRK-MODULO.
2912
2912       CALL WRK-MODULO          USING WAJS-ENTRADA
2912                                      WAJT-SAIDA
2912                                      GFCT0M-AREA-ERROS.
2912
2912       IF  RETURN-CODE             NOT EQUAL ZEROS
2912           MOVE WRK-MODULO         TO WRK-MODULO-MSG
2912           MOVE WRK-MSG05          TO GFCT0M-TEXTO
2912
2912           PERFORM 1100-FORMATAR-ERRO
2912
2912           PERFORM 9999-PROCESSAR-ROTINA-ERRO
2912       END-IF.
2912
2912       IF  WAJT-ERRO                EQUAL 9
2912           PERFORM 9999-PROCESSAR-ROTINA-ERRO
2912       END-IF.
2912
2912       PERFORM 6110-TRATA-RET-COMBO-GFCT5173
2912
2912       MOVE WAJT-SAIDA              TO GFCT0L-ENTRADA.
2912
2912       IF  WRK-FUNCAO               EQUAL WRK-RSTA
2912           MOVE 255                 TO GFCT3O-LL
2912           MOVE WAJT-FUNCAO         TO GFCT3O-FUNCAO
2912           MOVE WAJT-FILTRO         TO WAJS-FILTRO
2912           MOVE WAJT-QTDE-OCOR      TO WAJS-QTDE-OCOR
2912           MOVE ZEROS               TO WAJS-QTDE-TOT-REG
2912           MOVE WAJT-FUNC-BDSCO     TO WAJS-FUNC-BDSCO
2912           MOVE WAJT-PONTEIRO       TO WAJS-PONTEIRO
2912           MOVE WAJT-FIM            TO WAJS-FIM
2912           MOVE WAJS-DADOS-RST      TO GFCT3O-DADOS-RST
2912       END-IF.
2912
2912       PERFORM 2120-RETORNAR-WEB.
2912
2912  *----------------------------------------------------------------*
2912   6100-99-FIM.                    EXIT.
2912  *----------------------------------------------------------------*
2912
2912  ******************************************************************
2912  * TRATAR RETORNO COMBO DE SEGMENTO VIGENCIA                      *
2912  ******************************************************************
2912  *----------------------------------------------------------------*
2912   6110-TRATA-RET-COMBO-GFCT5173   SECTION.
2912  *----------------------------------------------------------------*
2912
2912       IF  WAJT-FIM                EQUAL 'S'
2912           MOVE WRK-MSGF           TO GFCT0L-TPOMSG-WEB
2912       ELSE
2912           IF  IND-1               LESS 3
2912               MOVE WRK-MSGI       TO GFCT0L-TPOMSG-WEB
2912               MOVE WAJT-FILTRO    TO WAJS-FILTRO
2912               MOVE WAJT-PONTEIRO  TO WAJS-PONTEIRO
2912           ELSE
2912               MOVE WRK-RSTA       TO GFCT0L-TPOMSG-WEB
2912           END-IF
2912       END-IF.
2912
2912       IF  GFCT0L-TPOMSG-WEB       EQUAL WRK-RSTA OR WRK-MSGF
2912           MOVE ZEROS              TO GFCT0L-NRO-MSGI
2912       ELSE
2912           MOVE GFCT0L-NRO-MSGI    TO WRK-NRO-MSGI
2912           ADD 1                   TO WRK-NRO-MSGI
2912           MOVE WRK-NRO-MSGI       TO GFCT0L-NRO-MSGI
2912       END-IF.
2912
2912       MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
2912       MOVE GFCT0L-TPOMSG-WEB      TO WRK-FUNCAO.
2912
2912  *----------------------------------------------------------------*
2912   6110-99-FIM.                    EXIT.
2912  *----------------------------------------------------------------*

2912  ******************************************************************
2912  * PROCESSAR COMBO DE UF'S  VIGENCIA                              *
2912  ******************************************************************
2912  *----------------------------------------------------------------*
2912   6200-PROCESSA-COMBO-GFCT5183    SECTION.
2912  *----------------------------------------------------------------*
2912
2912       IF  WAJW-FIM                EQUAL 'N'
2912           MOVE SPACES             TO WAJX-SAIDA
2912                                      GFCT0M-AREA-ERROS
2912
2912           INITIALIZE                 GFCT0L-RET-WEB
2912                                      WAJX-SAIDA
2912                                      GFCT3O-AREA-RESTART
2912                                      GFCT0M-AREA-ERROS
2912       END-IF.
2912
2912       MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
2912       MOVE WAJW-FUNCAO            TO WRK-MODULO.
2912
2912       CALL WRK-MODULO          USING WAJW-ENTRADA
2912                                      WAJX-SAIDA
2912                                      GFCT0M-AREA-ERROS.
2912
2912       IF  RETURN-CODE             NOT EQUAL ZEROS
2912           MOVE WRK-MODULO         TO WRK-MODULO-MSG
2912           MOVE WRK-MSG05          TO GFCT0M-TEXTO
2912
2912           PERFORM 1100-FORMATAR-ERRO
2912
2912           PERFORM 9999-PROCESSAR-ROTINA-ERRO
2912       END-IF.
2912
2912       IF  WAJX-ERRO                EQUAL 9
2912           PERFORM 9999-PROCESSAR-ROTINA-ERRO
2912       END-IF.
2912
2912       PERFORM 6210-TRATA-RET-COMBO-GFCT5183
2912
2912       MOVE WAJX-SAIDA              TO GFCT0L-ENTRADA.
2912
2912       IF  WRK-FUNCAO               EQUAL WRK-RSTA
2912           MOVE 255                 TO GFCT3O-LL
2912           MOVE WAJX-FUNCAO         TO GFCT3O-FUNCAO
2912           MOVE WAJX-FILTRO         TO WAJW-FILTRO
2912           MOVE WAJX-QTDE-OCOR      TO WAJW-QTDE-OCOR
2912           MOVE ZEROS               TO WAJW-QTDE-TOT-REG
2912           MOVE WAJX-FUNC-BDSCO     TO WAJW-FUNC-BDSCO
2912           MOVE WAJX-PONTEIRO       TO WAJW-PONTEIRO
2912           MOVE WAJX-FIM            TO WAJW-FIM
2912           MOVE WAJW-DADOS-RST      TO GFCT3O-DADOS-RST
2912       END-IF.
2912
2912       PERFORM 2120-RETORNAR-WEB.
2912
2912  *----------------------------------------------------------------*
2912   6200-99-FIM.                    EXIT.
2912  *----------------------------------------------------------------*
2912
2912  ******************************************************************
2912  * TRATAR RETORNO COMBO DE UF'S VIGENCIA                          *
2912  ******************************************************************
2912  *----------------------------------------------------------------*
2912   6210-TRATA-RET-COMBO-GFCT5183   SECTION.
2912  *----------------------------------------------------------------*
2912
2912       IF  WAJX-FIM                EQUAL 'S'
2912           MOVE WRK-MSGF           TO GFCT0L-TPOMSG-WEB
2912       ELSE
2912           IF  IND-1               LESS 3
2912               MOVE WRK-MSGI       TO GFCT0L-TPOMSG-WEB
2912               MOVE WAJX-FILTRO    TO WAJW-FILTRO
2912               MOVE WAJX-PONTEIRO  TO WAJW-PONTEIRO
2912           ELSE
2912               MOVE WRK-RSTA       TO GFCT0L-TPOMSG-WEB
2912           END-IF
2912       END-IF.
2912
2912       IF  GFCT0L-TPOMSG-WEB       EQUAL WRK-RSTA OR WRK-MSGF
2912           MOVE ZEROS              TO GFCT0L-NRO-MSGI
2912       ELSE
2912           MOVE GFCT0L-NRO-MSGI    TO WRK-NRO-MSGI
2912           ADD 1                   TO WRK-NRO-MSGI
2912           MOVE WRK-NRO-MSGI       TO GFCT0L-NRO-MSGI
2912       END-IF.
2912
2912       MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
2912       MOVE GFCT0L-TPOMSG-WEB      TO WRK-FUNCAO.
2912
2912  *----------------------------------------------------------------*
2912   6210-99-FIM.                    EXIT.
2912  *----------------------------------------------------------------*

2912  ******************************************************************
2912  * PROCESSAR COMBO OBTEM NOME DO CLIENTE                          *
2912  ******************************************************************
2912  *----------------------------------------------------------------*
2912   6300-PROCESSA-COMBO-GFCT5110    SECTION.
2912  *----------------------------------------------------------------*
2912
2912       IF  GFCTIU-FIM              EQUAL 'N'
2912           MOVE SPACES             TO GFCTIV-SAIDA
2912                                      GFCT0M-AREA-ERROS
2912
2912           INITIALIZE                 GFCT0L-RET-WEB
2912                                      GFCTIV-SAIDA
2912                                      GFCT3O-AREA-RESTART
2912                                      GFCT0M-AREA-ERROS
2912       END-IF.
2912
2912       MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
2912       MOVE GFCTIU-FUNCAO          TO WRK-MODULO.
2912
2912       CALL WRK-MODULO          USING GFCTIU-ENTRADA
2912                                      GFCTIV-SAIDA
2912                                      GFCT0M-AREA-ERROS.
2912
2912       IF  RETURN-CODE             NOT EQUAL ZEROS
2912           MOVE WRK-MODULO         TO WRK-MODULO-MSG
2912           MOVE WRK-MSG05          TO GFCT0M-TEXTO
2912
2912           PERFORM 1100-FORMATAR-ERRO
2912
2912           PERFORM 9999-PROCESSAR-ROTINA-ERRO
2912       END-IF.
2912
2912       IF  GFCTIV-ERRO              EQUAL 9
2912           PERFORM 9999-PROCESSAR-ROTINA-ERRO
2912       END-IF.
2912
2912       PERFORM 6310-TRATA-RET-COMBO-GFCT5110
2912
2912       MOVE GFCTIV-SAIDA            TO GFCT0L-ENTRADA.
2912
2912       IF  WRK-FUNCAO               EQUAL WRK-RSTA
2912           MOVE 255                 TO GFCT3O-LL
2912           MOVE GFCTIV-FUNCAO       TO GFCT3O-FUNCAO
2912           MOVE GFCTIV-QTDE-OCOR    TO GFCTIU-QTDE-OCOR
2912           MOVE ZEROS               TO GFCTIU-QTDE-TOT-REG
2912           MOVE GFCTIV-FUNC-BDSCO   TO GFCTIU-FUNC-BDSCO
ST2506*        MOVE GFCTIV-NRO-CPF-CNPJ TO GFCTIU-NRO-CPF-CNPJ
ST2506*        MOVE GFCTIV-NRO-FILIAL   TO GFCTIU-NRO-FILIAL
ST2506         MOVE GFCTIV-COD-CPF-CNPJ TO GFCTIU-COD-CPF-CNPJ
ST2506         MOVE GFCTIV-COD-FILIAL   TO GFCTIU-COD-FILIAL
2912           MOVE GFCTIV-CTRL-CPF-CNPJ
2912                                    TO GFCTIU-CTRL-CPF-CNPJ
2912           MOVE GFCTIV-FIM          TO GFCTIU-FIM
2912           MOVE GFCTIU-DADOS-RST    TO GFCT3O-DADOS-RST
2912       END-IF.
2912
2912       PERFORM 2120-RETORNAR-WEB.
2912
2912  *----------------------------------------------------------------*
2912   6300-99-FIM.                    EXIT.
2912  *----------------------------------------------------------------*
2912
2912  ******************************************************************
2912  * TRATAR RETORNO COMBO DE NOME CLIENTES                          *
2912  ******************************************************************
2912  *----------------------------------------------------------------*
2912   6310-TRATA-RET-COMBO-GFCT5110   SECTION.
2912  *----------------------------------------------------------------*
2912
2912       IF  GFCTIV-FIM           EQUAL 'S'
2912           MOVE WRK-MSGF           TO GFCT0L-TPOMSG-WEB
2912       ELSE
2912           IF  IND-1               LESS 3
2912               MOVE WRK-MSGI       TO GFCT0L-TPOMSG-WEB
2912           ELSE
2912               MOVE WRK-RSTA       TO GFCT0L-TPOMSG-WEB
2912           END-IF
2912       END-IF.
2912
2912       IF  GFCT0L-TPOMSG-WEB       EQUAL WRK-RSTA OR WRK-MSGF
2912           MOVE ZEROS              TO GFCT0L-NRO-MSGI
2912       ELSE
2912           MOVE GFCT0L-NRO-MSGI    TO WRK-NRO-MSGI
2912           ADD 1                   TO WRK-NRO-MSGI
2912           MOVE WRK-NRO-MSGI       TO GFCT0L-NRO-MSGI
2912       END-IF.
2912
2912       MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
2912       MOVE GFCT0L-TPOMSG-WEB      TO WRK-FUNCAO.
2912
2912  *----------------------------------------------------------------*
2912   6310-99-FIM.                    EXIT.
2912  *----------------------------------------------------------------*

0610XS******************************************************************
0610XS* PROCESSAR TEXT DE BANCO POSTAL / BRADESCO EXPRESSO             *
0610XS******************************************************************
0610XS*----------------------------------------------------------------*
0610XS 6400-PROCESSAR-TEXT-POSTAL-EXP  SECTION.
0610XS*----------------------------------------------------------------*
0610XS
0610XS     IF  WAM2-FIM                EQUAL 'N'
0610XS         MOVE SPACES             TO WAM3-SAIDA
0610XS                                    GFCT0M-AREA-ERROS
0610XS
0610XS         INITIALIZE              GFCT0L-RET-WEB
0610XS                                 WAM3-SAIDA
0610XS                                 GFCT0M-AREA-ERROS
0610XS     END-IF.
0610XS
0610XS     MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
0610XS
0610XS     MOVE WAM2-FUNCAO            TO WRK-MODULO.
0610XS
0610XS     CALL WRK-MODULO             USING WAM2-ENTRADA
0610XS                                       WAM3-SAIDA
0610XS                                       GFCT0M-AREA-ERROS.
0610XS
0610XS     IF  RETURN-CODE             NOT EQUAL ZEROS
0610XS         MOVE WRK-MODULO         TO WRK-MODULO-MSG
0610XS         MOVE WRK-MSG05          TO GFCT0M-TEXTO
0610XS
0610XS         PERFORM 1100-FORMATAR-ERRO
0610XS
0610XS         PERFORM 9999-PROCESSAR-ROTINA-ERRO
0610XS     END-IF.
0610XS
0610XS     IF  WAM3-ERRO               EQUAL 9
0610XS         PERFORM 9999-PROCESSAR-ROTINA-ERRO
0610XS     END-IF.
0610XS
0610XS     PERFORM 2210-TRATAR-RETORNO-COMUM.
0610XS
0610XS     MOVE WAM3-SAIDA             TO GFCT0L-ENTRADA.
0610XS
0610XS     PERFORM 2120-RETORNAR-WEB.
0610XS
0610XS*----------------------------------------------------------------*
0610XS 6400-99-FIM.                    EXIT.
0610XS*----------------------------------------------------------------*

0710XS******************************************************************
0710XS* PROCESSAR TEXT DE RAZAO CONTABIL                               *
0710XS******************************************************************
0710XS*----------------------------------------------------------------*
0710XS 6500-PROCESSAR-TEXT-RZ-CONTAB   SECTION.
0710XS*----------------------------------------------------------------*
0710XS
0710XS     IF  WANV-FIM                EQUAL 'N'
0710XS         MOVE SPACES             TO WANW-SAIDA
0710XS                                    GFCT0M-AREA-ERROS
0710XS
0710XS         INITIALIZE              GFCT0L-RET-WEB
0710XS                                 WANW-SAIDA
0710XS                                 GFCT0M-AREA-ERROS
0710XS     END-IF.
0710XS
0710XS     MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
0710XS
0710XS     MOVE WANV-FUNCAO            TO WRK-MODULO.
0710XS
0710XS     CALL WRK-MODULO             USING WANV-ENTRADA
0710XS                                       WANW-SAIDA
0710XS                                       GFCT0M-AREA-ERROS.
0710XS
0710XS     IF  RETURN-CODE             NOT EQUAL ZEROS
0710XS         MOVE WRK-MODULO         TO WRK-MODULO-MSG
0710XS         MOVE WRK-MSG05          TO GFCT0M-TEXTO
0710XS
0710XS         PERFORM 1100-FORMATAR-ERRO
0710XS
0710XS         PERFORM 9999-PROCESSAR-ROTINA-ERRO
0710XS     END-IF.
0710XS
0710XS     IF  WANW-ERRO               EQUAL 9
0710XS         PERFORM 9999-PROCESSAR-ROTINA-ERRO
0710XS     END-IF.
0710XS
0710XS     PERFORM 2210-TRATAR-RETORNO-COMUM.
0710XS
0710XS     MOVE WANW-SAIDA             TO GFCT0L-ENTRADA.
0710XS
0710XS     PERFORM 2120-RETORNAR-WEB.
0710XS
0710XS*----------------------------------------------------------------*
0710XS 6500-99-FIM.                    EXIT.
0710XS*----------------------------------------------------------------*

0710XS******************************************************************
0710XS* PROCESSAR COMBO DE RAZAO CONTABIL                              *
0710XS******************************************************************
0710XS*----------------------------------------------------------------*
0710XS 6600-PROCESSAR-COMBO-RZ-CONTAB  SECTION.
0710XS*----------------------------------------------------------------*
0710XS
0710XS     IF  WANX-FIM                EQUAL 'N'
0710XS         MOVE SPACES             TO WANY-SAIDA
0710XS                                    GFCT0M-AREA-ERROS
0710XS
0710XS         INITIALIZE                 GFCT0L-RET-WEB
0710XS                                    WANY-SAIDA
0710XS                                    GFCT3O-AREA-RESTART
0710XS                                    GFCT0M-AREA-ERROS
0710XS     END-IF.
0710XS
0710XS     MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
0710XS     MOVE WANX-FUNCAO            TO WRK-MODULO.
0710XS
0710XS     CALL WRK-MODULO          USING WANX-ENTRADA
0710XS                                    WANY-SAIDA
0710XS                                    GFCT0M-AREA-ERROS.
0710XS
0710XS     IF  RETURN-CODE             NOT EQUAL ZEROS
0710XS         MOVE WRK-MODULO         TO WRK-MODULO-MSG
0710XS         MOVE WRK-MSG05          TO GFCT0M-TEXTO
0710XS
0710XS         PERFORM 1100-FORMATAR-ERRO
0710XS
0710XS         PERFORM 9999-PROCESSAR-ROTINA-ERRO
0710XS     END-IF.
0710XS
0710XS     IF  WANY-ERRO               EQUAL 9
0710XS         PERFORM 9999-PROCESSAR-ROTINA-ERRO
0710XS     END-IF.
0710XS
0710XS     PERFORM 6610-TRATA-RET-COMBO-RZ-CONTAB
0710XS
0710XS     MOVE WANY-SAIDA             TO GFCT0L-ENTRADA.
0710XS
0710XS     IF  WRK-FUNCAO              EQUAL WRK-RSTA
0710XS         MOVE 255                TO GFCT3O-LL
0710XS         MOVE WANY-FUNCAO        TO GFCT3O-FUNCAO
0710XS         MOVE WANY-FILTRO-CAGPTO-CTA
0710XS                                 TO WANX-FILTRO-CAGPTO-CTA
0710XS         MOVE WANY-FILTRO-GRP    TO WANX-FILTRO-GRP
0710XS         MOVE WANY-FILTRO-SGRP   TO WANX-FILTRO-SGRP
0710XS         MOVE WANY-QTDE-OCOR     TO WANX-QTDE-OCOR
0710XS         MOVE ZEROS              TO WANX-QTDE-TOT-REG
0710XS         MOVE WANY-FUNC-BDSCO    TO WANX-FUNC-BDSCO
0710XS         MOVE WANY-PONTEIRO      TO WANX-PONTEIRO
0710XS         MOVE WANY-FIM           TO WANX-FIM
0710XS         MOVE WANX-DADOS-RST     TO GFCT3O-DADOS-RST
0710XS     END-IF.
0710XS
0710XS     PERFORM 2120-RETORNAR-WEB.
0710XS
0710XS*----------------------------------------------------------------*
0710XS 6600-99-FIM.                    EXIT.
0710XS*----------------------------------------------------------------*

0710XS******************************************************************
0710XS* TRATAR RETORNO COMBO DE RAZAO CONTABIL                         *
0710XS******************************************************************
0710XS*----------------------------------------------------------------*
0710XS 6610-TRATA-RET-COMBO-RZ-CONTAB  SECTION.
0710XS*----------------------------------------------------------------*
0710XS
0710XS     IF  WANY-FIM                EQUAL 'S'
0710XS         MOVE WRK-MSGF           TO GFCT0L-TPOMSG-WEB
0710XS     ELSE
0710XS         IF  IND-1               LESS 3
0710XS             MOVE WRK-MSGI       TO GFCT0L-TPOMSG-WEB
0710XS
0710XS             MOVE WANY-FILTRO-CAGPTO-CTA
0710XS                                 TO WANX-FILTRO-CAGPTO-CTA
0710XS             MOVE WANY-FILTRO-GRP
0710XS                                 TO WANX-FILTRO-GRP
0710XS             MOVE WANY-FILTRO-SGRP
0710XS                                 TO WANX-FILTRO-SGRP
0710XS             MOVE WANY-PONTEIRO  TO WANX-PONTEIRO
0710XS         ELSE
0710XS             MOVE WRK-RSTA       TO GFCT0L-TPOMSG-WEB
0710XS         END-IF
0710XS     END-IF.
0710XS
0710XS     IF  GFCT0L-TPOMSG-WEB       EQUAL WRK-RSTA OR WRK-MSGF
0710XS         MOVE ZEROS              TO GFCT0L-NRO-MSGI
0710XS     ELSE
0710XS         MOVE GFCT0L-NRO-MSGI    TO WRK-NRO-MSGI
0710XS         ADD 1                   TO WRK-NRO-MSGI
0710XS         MOVE WRK-NRO-MSGI       TO GFCT0L-NRO-MSGI
0710XS     END-IF.
0710XS
0710XS     MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
0710XS     MOVE GFCT0L-TPOMSG-WEB      TO WRK-FUNCAO.
0710XS
0710XS*----------------------------------------------------------------*
0710XS 6610-99-FIM.                    EXIT.
0710XS*----------------------------------------------------------------*

0710XS******************************************************************
0710XS* PROCESSAR COMBO DE RAZAO CONTABIL                              *
0710XS******************************************************************
0710XS*----------------------------------------------------------------*
0710XS 6700-PROCESSAR-COMBO-RZ-CONTAB  SECTION.
0710XS*----------------------------------------------------------------*
0710XS
0710XS     IF  WANZ-FIM                EQUAL 'N'
0710XS         MOVE SPACES             TO WAN0-SAIDA
0710XS                                    GFCT0M-AREA-ERROS
0710XS
0710XS         INITIALIZE                 GFCT0L-RET-WEB
0710XS                                    WAN0-SAIDA
0710XS                                    GFCT3O-AREA-RESTART
0710XS                                    GFCT0M-AREA-ERROS
0710XS     END-IF.
0710XS
0710XS     MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
0710XS     MOVE WANZ-FUNCAO            TO WRK-MODULO.
0710XS
0710XS     CALL WRK-MODULO          USING WANZ-ENTRADA
0710XS                                    WAN0-SAIDA
0710XS                                    GFCT0M-AREA-ERROS.
0710XS
0710XS     IF  RETURN-CODE             NOT EQUAL ZEROS
0710XS         MOVE WRK-MODULO         TO WRK-MODULO-MSG
0710XS         MOVE WRK-MSG05          TO GFCT0M-TEXTO
0710XS
0710XS         PERFORM 1100-FORMATAR-ERRO
0710XS
0710XS         PERFORM 9999-PROCESSAR-ROTINA-ERRO
0710XS     END-IF.
0710XS
0710XS     IF  WAN0-ERRO               EQUAL 9
0710XS         PERFORM 9999-PROCESSAR-ROTINA-ERRO
0710XS     END-IF.
0710XS
0710XS     PERFORM 6710-TRATA-RET-COMBO-RZ-CONTAB
0710XS
0710XS     MOVE WAN0-SAIDA             TO GFCT0L-ENTRADA.
0710XS
0710XS     IF  WRK-FUNCAO              EQUAL WRK-RSTA
0710XS         MOVE 255                TO GFCT3O-LL
0710XS         MOVE WAN0-FUNCAO        TO GFCT3O-FUNCAO
0710XS         MOVE WAN0-QTDE-OCOR     TO WANZ-QTDE-OCOR
0710XS         MOVE ZEROS              TO WANZ-QTDE-TOT-REG
0710XS         MOVE WAN0-FUNC-BDSCO    TO WANZ-FUNC-BDSCO
0710XS         MOVE WAN0-FILTRO        TO WANZ-FILTRO
0710XS         MOVE WAN0-PONTEIRO      TO WANZ-PONTEIRO
0710XS         MOVE WAN0-FIM           TO WANZ-FIM
0710XS         MOVE WANZ-DADOS-RST     TO GFCT3O-DADOS-RST
0710XS     END-IF.
0710XS
0710XS     PERFORM 2120-RETORNAR-WEB.
0710XS
0710XS*----------------------------------------------------------------*
0710XS 6700-99-FIM.                    EXIT.
0710XS*----------------------------------------------------------------*

0710XS******************************************************************
0710XS* TRATAR RETORNO COMBO DE RAZAO CONTABIL                         *
0710XS******************************************************************
0710XS*----------------------------------------------------------------*
0710XS 6710-TRATA-RET-COMBO-RZ-CONTAB  SECTION.
0710XS*----------------------------------------------------------------*
0710XS
0710XS     IF  WAN0-FIM                EQUAL 'S'
0710XS         MOVE WRK-MSGF           TO GFCT0L-TPOMSG-WEB
0710XS     ELSE
0710XS         IF  IND-1               LESS 3
0710XS             MOVE WRK-MSGI       TO GFCT0L-TPOMSG-WEB
0710XS
0710XS             MOVE WAN0-FILTRO    TO WANZ-FILTRO
0710XS             MOVE WAN0-PONTEIRO  TO WANZ-PONTEIRO
0710XS         ELSE
0710XS             MOVE WRK-RSTA       TO GFCT0L-TPOMSG-WEB
0710XS         END-IF
0710XS     END-IF.
0710XS
0710XS     IF  GFCT0L-TPOMSG-WEB       EQUAL WRK-RSTA OR WRK-MSGF
0710XS         MOVE ZEROS              TO GFCT0L-NRO-MSGI
0710XS     ELSE
0710XS         MOVE GFCT0L-NRO-MSGI    TO WRK-NRO-MSGI
0710XS         ADD 1                   TO WRK-NRO-MSGI
0710XS         MOVE WRK-NRO-MSGI       TO GFCT0L-NRO-MSGI
0710XS     END-IF.
0710XS
0710XS     MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
0710XS     MOVE GFCT0L-TPOMSG-WEB      TO WRK-FUNCAO.
0710XS
0710XS*----------------------------------------------------------------*
0710XS 6710-99-FIM.                    EXIT.
0710XS*----------------------------------------------------------------*

0710XS******************************************************************
0710XS* PROCESSAR COMBO DE RAZAO CONTABIL                              *
0710XS******************************************************************
0710XS*----------------------------------------------------------------*
0710XS 6800-PROCESSAR-COMBO-RZ-CONTAB  SECTION.
0710XS*----------------------------------------------------------------*
0710XS
0710XS     IF  WAN1-FIM                EQUAL 'N'
0710XS         MOVE SPACES             TO WAN2-SAIDA
0710XS                                    GFCT0M-AREA-ERROS
0710XS
0710XS         INITIALIZE                 GFCT0L-RET-WEB
0710XS                                    WAN2-SAIDA
0710XS                                    GFCT3O-AREA-RESTART
0710XS                                    GFCT0M-AREA-ERROS
0710XS     END-IF.
0710XS
0710XS     MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
0710XS     MOVE WAN1-FUNCAO            TO WRK-MODULO.
0710XS
0710XS     CALL WRK-MODULO          USING WAN1-ENTRADA
0710XS                                    WAN2-SAIDA
0710XS                                    GFCT0M-AREA-ERROS.
0710XS
0710XS     IF  RETURN-CODE             NOT EQUAL ZEROS
0710XS         MOVE WRK-MODULO         TO WRK-MODULO-MSG
0710XS         MOVE WRK-MSG05          TO GFCT0M-TEXTO
0710XS
0710XS         PERFORM 1100-FORMATAR-ERRO
0710XS
0710XS         PERFORM 9999-PROCESSAR-ROTINA-ERRO
0710XS     END-IF.
0710XS
0710XS     IF  WAN2-ERRO               EQUAL 9
0710XS         PERFORM 9999-PROCESSAR-ROTINA-ERRO
0710XS     END-IF.
0710XS
0710XS     PERFORM 6810-TRATA-RET-COMBO-RZ-CONTAB
0710XS
0710XS     MOVE WAN2-SAIDA             TO GFCT0L-ENTRADA.
0710XS
0710XS     IF  WRK-FUNCAO              EQUAL WRK-RSTA
0710XS         MOVE 255                TO GFCT3O-LL
0710XS         MOVE WAN2-FUNCAO        TO GFCT3O-FUNCAO
0710XS         MOVE WAN2-QTDE-OCOR     TO WAN1-QTDE-OCOR
0710XS         MOVE ZEROS              TO WAN1-QTDE-TOT-REG
0710XS         MOVE WAN2-FUNC-BDSCO    TO WAN1-FUNC-BDSCO
0710XS         MOVE WAN2-FILTRO        TO WAN1-FILTRO
0710XS         MOVE WAN2-PONTEIRO      TO WAN1-PONTEIRO
0710XS         MOVE WAN2-FIM           TO WAN1-FIM
0710XS         MOVE WAN1-DADOS-RST     TO GFCT3O-DADOS-RST
0710XS     END-IF.
0710XS
0710XS     PERFORM 2120-RETORNAR-WEB.
0710XS
0710XS*----------------------------------------------------------------*
0710XS 6800-99-FIM.                    EXIT.
0710XS*----------------------------------------------------------------*

0710XS******************************************************************
0710XS* TRATAR RETORNO COMBO DE RAZAO CONTABIL                         *
0710XS******************************************************************
0710XS*----------------------------------------------------------------*
0710XS 6810-TRATA-RET-COMBO-RZ-CONTAB  SECTION.
0710XS*----------------------------------------------------------------*
0710XS
0710XS     IF  WAN2-FIM                EQUAL 'S'
0710XS         MOVE WRK-MSGF           TO GFCT0L-TPOMSG-WEB
0710XS     ELSE
0710XS         IF  IND-1               LESS 3
0710XS             MOVE WRK-MSGI       TO GFCT0L-TPOMSG-WEB
0710XS
0710XS             MOVE WAN2-FILTRO    TO WAN1-FILTRO
0710XS             MOVE WAN2-PONTEIRO  TO WAN1-PONTEIRO
0710XS         ELSE
0710XS             MOVE WRK-RSTA       TO GFCT0L-TPOMSG-WEB
0710XS         END-IF
0710XS     END-IF.
0710XS
0710XS     IF  GFCT0L-TPOMSG-WEB       EQUAL WRK-RSTA OR WRK-MSGF
0710XS         MOVE ZEROS              TO GFCT0L-NRO-MSGI
0710XS     ELSE
0710XS         MOVE GFCT0L-NRO-MSGI    TO WRK-NRO-MSGI
0710XS         ADD 1                   TO WRK-NRO-MSGI
0710XS         MOVE WRK-NRO-MSGI       TO GFCT0L-NRO-MSGI
0710XS     END-IF.
0710XS
0710XS     MOVE '00'                   TO GFCT0L-MPARCODE-APLIC.
0710XS     MOVE GFCT0L-TPOMSG-WEB      TO WRK-FUNCAO.
0710XS
0710XS*----------------------------------------------------------------*
0710XS 6810-99-FIM.                    EXIT.
0710XS*----------------------------------------------------------------*

      ******************************************************************
      * TRATAR ERROS OCORRIDOS DURANTE PROCESSAMENTO                   *
      ******************************************************************
      *----------------------------------------------------------------*
       9999-PROCESSAR-ROTINA-ERRO      SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT7407'             TO GFCT0M-PROGRAMA.
           MOVE GFCT0L-MPARCODE-APLIC  TO MPA-RCODE-APLICACAO

           IF  GFCT0M-SQLCA-AREA       EQUAL SPACES OR LOW-VALUES
               CALL 'SBAT3000'         USING WRK-ERRO
                                             IO-PCB
                                             ALT-PCB
                                             MCA-ISDPA
                                             GFCT0M-ERRO-AREAS
           ELSE
               CALL 'SBAT3000'         USING WRK-ERRO
                                             IO-PCB
                                             ALT-PCB
                                             MCA-ISDPA
                                             GFCT0M-ERRO-AREAS
                                             GFCT0M-SQLCA-AREA
           END-IF.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
