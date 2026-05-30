
      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. DCOM1801.
       AUTHOR.     MARCOS TADEU.
      *================================================================*
      *                          A L T R A N                           *
      *----------------------------------------------------------------*
      *    PROGRAMA....:  DCOM1801                                     *
      *    ANALISTA....:  MARCOS TADEU             - ALTRAN / GRUPO 70 *
      *    DATA........:  21/04/2007                                   *
      *----------------------------------------------------------------*
      *    OBJETIVO....:  PRORROGACAO DE VENCIMENTO - MENU             *
      *----------------------------------------------------------------*
      *    TELA........:  DCOMRATM - PRORROGACAO VENCIMENTO - MENU     *
      *----------------------------------------------------------------*
      *    INC'S.......:                                               *
      *    I#BRAD7C - AREA DE COMUNICACAO COM A BRAD7100               *
      *----------------------------------------------------------------*

      *    MODULOS.....:                                               *
      *    BRAD0110 - CALCULAR CONTROLE DO CGC/CPF                     *
      *    BRAD0660 - INSERE CARACTERES DE NULOS NA MENSAGEM DE SAIDA  *
      *    BRAD5000 - ENVIA / RECEBE MENSAGENS DO IMS                  *
      *    BRAD7100 - TRATAMENTO DE ERROS                              *
      *    BRAD7600 - OBTEM DATA E HORA DO SISTEMA                     *
      *----------------------------------------------------------------*
      *    NAVEGACAO...:                                               *
      *    CHAMADO POR.: DCOM1801                   CHAMA: DCOM1801    *
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
           '*** INICIO DA WORKING STORAGE SECTION ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA ATRIBUTOS ***'.
      *----------------------------------------------------------------*

      *------ PROTEGIDO,  NORMAL, ALFA -------------------------*
       77  WRK-193                     PIC S9(008) COMP    VALUE +193.

      *------ PROTEGIDO,  NORMAL, NUMERICO ---------------------*
       77  WRK-241                     PIC S9(008) COMP    VALUE +241.
      *------ DESPROTEGIDO, NORMAL, ALFA, POS CURSOR --------*
       77  WRK-49345                   PIC S9(008) COMP    VALUE +49345.
      *------ DESPROTEGIDO, BRILHANTE, ALFA, POS CURSOR --------*
       77  WRK-49353                   PIC S9(008) COMP    VALUE +49353.
      *------ DESPROTEGIDO, BRILHANTE, NUMERICO, POS CURSOR --------*
       77  WRK-49369                   PIC S9(008) COMP    VALUE +49369.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREAS AUXILIARES ***'.
      *----------------------------------------------------------------*

       77  WRK-MODULO                  PIC  X(008)         VALUE SPACES.
       77  WRK-INCONSIS                PIC  X(001)         VALUE SPACES.
       77  WRK-CHAMOU-4170             PIC  X(001)         VALUE SPACES.
       77  WRK-ACU-SEL                 PIC  9(003)         VALUE ZEROS.

       01  WRK-COD-RET                 PIC +9(004)         VALUE ZEROS.
       01  WRK-COD-RET-R               REDEFINES           WRK-COD-RET.

           05  FILLER                  PIC  X(003).
           05  WRK-COD-RET-N           PIC  9(002).

       01  WRK-INPUT-ENTRADA.
           05  WRK-INPUT-AUX           PIC  X(0282)        VALUE SPACES.
           05  FILLER                  PIC  X(1718)        VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREAS DA BRAD5000 ***'.
      *----------------------------------------------------------------*

       01  WRK-SQLCA                  PIC  X(136)         VALUE SPACES.
       01  WRK-GU                      PIC  X(004)         VALUE 'GU'.
       01  WRK-CHNG                    PIC  X(004)         VALUE 'CHNG'.
       01  WRK-ISRT                    PIC  X(004)         VALUE 'ISRT'.
       01  WRK-COD-USER.
           05  WRK-COD-USER-N          PIC  9(007)         VALUE ZEROS.
       01  WRK-FUNCAO                  PIC  X(004)         VALUE SPACES.
       01  WRK-TELA                    PIC  X(008)         VALUE SPACES.
       01  WRK-VERSAO                  PIC  X(006)         VALUE

           'VRS002'.

       01  WRK-COD-DEPTO.
           05  FILLER                  PIC  X(002)         VALUE SPACES.
           05  WRK-COD-DEPTO-N         PIC  9(004)         VALUE ZEROS.

       01  WRK-MENSAGEM.
           05  FILLER                  PIC  X(004)         VALUE SPACES.
           05  WRK-TRANSACAO           PIC  X(008)         VALUE SPACES.
           05  FILLER                  PIC  X(1988)        VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE COMUNICACAO - DCOM1501 ***'.
      *----------------------------------------------------------------*

       01  WRK-COMU-AREA-1501.
           05  WRK-COMU-LL-1501        PIC S9(004) COMP    VALUE +169.
           05  WRK-COMU-ZZ-1501        PIC S9(004) COMP    VALUE ZEROS.
           05  WRK-COMU-TRANCODE-1501.
               10  WRK-COMU-TRANSACAO-1501

                                       PIC  X(008)         VALUE SPACES.
               10  FILLER              PIC  X(006)         VALUE SPACES.
               10  WRK-COMU-PFK-1501   PIC  X(001)         VALUE SPACES.
           05  WRK-COMU-SENHAS-1501    PIC  X(037)         VALUE SPACES.
           05  WRK-COMU-COMANDO-1501   PIC  X(068)         VALUE SPACES.
           05  WRK-COMU-DADOS-1501.
               10  WRK-COMU-OPCAO-1501 PIC  9(001)         VALUE ZEROS.
               10  WRK-COMU-TIMESTAMP-1501
                                       PIC  X(026)         VALUE SPACES.
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*** AREA PARA DCOM6438 ***'.
      *----------------------------------------------------------------*

       COPY 'I#DCOMXF'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*** AREA PARA DCOM5484 ***'.
      *----------------------------------------------------------------*
       01  ROTENT-5484-ENVIO.

           05 ROTENT-5484-COD-RETORNO  PIC  X(004).
           05 ROTENT-5484-MSG-RETORNO  PIC  X(079).
           05 ROTENT-5484-RESTART      PIC  9(005).
           05 ROTENT-5484-FLAG         PIC  X(001).
           05 ROTENT-5484-CFUNC-ENV    PIC  9(009).
           05 ROTENT-5484-CTERM        PIC  X(008).

       01  ROTSAI-5484-RETORNO.
           05 ROTSAI-5484-COD-RET      PIC  9(004).
           05 ROTSAI-5484-MENS-RET     PIC  X(079).
           05 ROTSAI-5484-RESTART      PIC  9(005).
           05 ROTSAI-5484-FLAG         PIC  X(001).
           05 ROTSAI-5484-DT-ATUAL     PIC  X(010).
           05 ROTSAI-5484-DT-RETR-ANT  PIC  X(010).
           05 ROTSAI-5484-RESUL-RET    PIC  9(001).
      *          1 - HORARIA ATUAL ANTERIOR AO RETROATIVO
      *          2 - HORARIO ATUAL POSTERIOR AO RETROATIVO

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE COMUNICACAO - DCOM1503 ***'.

      *----------------------------------------------------------------*

       01  WRK-COMU-AREA-15UN.
            05  WRK-COMU-LL-15UN       PIC S9(004) COMP    VALUE +0.
            05  WRK-COMU-ZZ-15UN       PIC S9(004) COMP    VALUE ZEROS.
            05  WRK-COMU-TRANCODE-15UN.
                10  WRK-COMU-TRANSACAO-15UN  PIC  X(008)  VALUE SPACES.
                10  FILLER                   PIC  X(006)  VALUE SPACES.
                10  WRK-COMU-PFK-15UN        PIC  X(001)  VALUE SPACES.
            05  WRK-COMU-SENHAS-15UN         PIC  X(037)  VALUE SPACES.
            05  WRK-COMU-COMANDO-15UN        PIC  X(068)  VALUE SPACES.
            05  WRK-COMU-DADOS-15UN.
                10  WRK-COMU-TPMANUT-15UN    PIC  X(001)  VALUE SPACES.
                10  WRK-COMU-TIMESTAMP-15UN  PIC  X(026)  VALUE SPACES.
                10  WRK-COMU-OPER-15UN.
                    15  WRK-COMU-DANOOPER-15UN
                                             PIC  9(004)  VALUE ZEROS.
                    15  WRK-COMU-NSEQOPER-15UN
                                             PIC  9(009)  VALUE ZEROS.
                10 WRK-COMU-PARCELA-15UN     PIC  9(005)  VALUE ZEROS.
                10 WRK-COMU-BANCO-15UN       PIC  9(003)  VALUE ZEROS.

                10  WRK-COMU-AGENC-15UN      PIC  9(005)  VALUE ZEROS.
                10  WRK-COMU-CONTA-15UN      PIC  9(013)  VALUE ZEROS.
4S2511*         10  WRK-COMU-CNPJCPF-15UN    PIC  9(009)  VALUE ZEROS.
                10  WRK-COMU-CNPJCPF-15UN    PIC  X(009)  VALUE SPACES.
4S2511*         10  WRK-COMU-FILIAL-15UN     PIC  9(004)  VALUE ZEROS.
                10  WRK-COMU-FILIAL-15UN     PIC  X(04)   VALUE SPACES.
                10  WRK-COMU-CONTROL-15UN    PIC  9(002)  VALUE ZEROS.
                10  WRK-COMU-CPRODUTO-15UN   PIC  9(003)  VALUE ZEROS.
                10  WRK-COMU-IPRODUTO-15UN   PIC  X(025)  VALUE SPACES.
                10  WRK-COMU-CSUBPROD-15UN   PIC  9(003)  VALUE ZEROS.
                10  WRK-COMU-ISUBPROD-15UN   PIC  X(025)  VALUE SPACES.
                10  WRK-COMU-PAGINA-1-15UN   PIC  9(004)  VALUE ZEROS.
                10  WRK-COMU-PAGINA-2-15UN   PIC  9(004)  VALUE ZEROS.
                10  WRK-COMU-TP-SELECAO-15UN PIC  9(001)  VALUE ZEROS.
                10  WRK-COMU-MENSA-15UN      PIC  X(079)  VALUE SPACES.

      *----------------------------------------------------------------*
       01  COMU-AREA-PFK.
           05  COMU-LL-PFK             PIC S9(04) COMP     VALUE +124.
           05  COMU-ZZ-PFK             PIC S9(04) COMP     VALUE ZEROS.
           05  COMU-TRANCODE.
               10  COMU-TRANSACAO-PFK  PIC  X(08)          VALUE SPACES.
               10  FILLER              PIC  X(06)          VALUE SPACES.

               10  COMU-PFK            PIC  X(01)          VALUE SPACES.
           05  COMU-SENHAS-PFK         PIC  X(37)          VALUE SPACES.
           05  COMU-COMANDO-PFK        PIC  X(68)          VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE ERRO DA BRAD0660 ***'.
      *----------------------------------------------------------------*

       01  WRK-ERRO-BRAD0660.
           05  FILLER                  PIC  X(045)         VALUE
               '* RETURN-CODE DIFERENTE DE ZEROS NA BRAD0660 '.
           05  FILLER                  PIC  X(012)         VALUE
               '- RET.COD = '.
           05  WRK-RETURN-CODE         PIC +9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(011)         VALUE
               ' - LOCAL = '.
           05  WRK-LOCAL-ERRO          PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(002)         VALUE ' *'.

      *---------------------------------------------------------------*

       01  FILLER                      PIC X(050)          VALUE
           '*** TELA DCOMRATM - INPUT ***'.
      *---------------------------------------------------------------*

       01  WRK-INPUT-RATM.
           05 FILLER                      PIC  X(018)      VALUE SPACES.
           05 WRK-INP-DADOS-RATM.
             10 WRK-INP-PFK-RATM          PIC  X(001)      VALUE SPACES.
             10 WRK-INP-SENHAS-RATM       PIC  X(037)      VALUE SPACES.
             10 WRK-INP-COMANDO-RATM      PIC  X(068)      VALUE SPACES.
             10 WRK-INP-COPCAO-RATM       PIC  X(001)      VALUE SPACES.
             10 WRK-INP-TIMETP-RATM       PIC  X(026)      VALUE SPACES.
             10 WRK-INP-OPCAO-RATM.
               15 WRK-INP-OPCAO-RATM-N    PIC  9(001)      VALUE ZEROS.
             10 WRK-INP-DANOOPER-RATM.
               15 WRK-INP-DANOOPER-RATM-N PIC  9(004)      VALUE ZEROS.
             10 WRK-INP-NSEQOPER-RATM.
               15 WRK-INP-NSEQOPER-RATM-N PIC  9(009)      VALUE ZEROS.
             10 WRK-INP-PARC-RATM.
               15 WRK-INP-PARC-RATM-N     PIC  9(005)      VALUE ZEROS.
             10 WRK-INP-AGENCIA-RATM.

               15 WRK-INP-AGENCIA-RATM-N  PIC  9(005)      VALUE ZEROS.
               10 WRK-INP-CNPJ-RATM.
4S2511*        15 WRK-INP-CNPJ-RATM-N     PIC  9(009)      VALUE ZEROS.
               15 WRK-INP-CNPJ-RATM-N     PIC  9(009)      VALUE ZEROS.
4S2511         15 WRK-INP-CNPJ-RATM-ST    PIC  X(009)      VALUE SPACES.
             10 WRK-INP-FILIAL-RATM.
4S2511*        15 WRK-INP-FILIAL-RATM-N   PIC  9(004)      VALUE ZEROS.
               15 WRK-INP-FILIAL-RATM-N   PIC  9(004)      VALUE ZEROS.
4S2511         15 WRK-INP-FILIAL-RATM-ST  PIC  X(04)       VALUE SPACES.
             10 WRK-INP-CTRL-RATM.
               15 WRK-INP-CTRL-RATM-N     PIC  9(002)      VALUE ZEROS.
             10 WRK-INP-AGEN2-RATM.
               15 WRK-INP-AGEN2-RATM-N    PIC  9(005)      VALUE ZEROS.
             10 WRK-INP-CONTA-RATM.
               15 WRK-INP-CONTA-RATM-N    PIC  9(013)      VALUE ZEROS.
             10 WRK-INP-MENSA-RATM        PIC  X(079)      VALUE SPACES.

      *---------------------------------------------------------------*
       01  FILLER                      PIC X(050)          VALUE
           '*** TELA DCOMRATM - OUTPUT ***'.
      *---------------------------------------------------------------*

       01  WRK-OUTPUT-RATM.
           05 WRK-OUT-LL-RATM             PIC S9(004) COMP VALUE +0294.
           05 WRK-OUT-ZZ-RATM             PIC  9(004) COMP VALUE ZEROS.

           05 WRK-OUT-DADOS-RATM.
             10 WRK-OUT-SENHAS-RATM       PIC  X(037)      VALUE SPACES.
             10 WRK-OUT-COMANDO-ATTR-RATM PIC  9(004) COMP VALUE ZEROS.
             10 WRK-OUT-COMANDO-RATM      PIC  X(068)      VALUE SPACES.
             10 WRK-OUT-COPCAO-RATM       PIC  X(001)      VALUE SPACES.
             10 WRK-OUT-TIMETP-RATM       PIC  X(026)      VALUE SPACES.
             10 WRK-OUT-OPCAO-ATTR-RATM   PIC  9(004) COMP VALUE ZEROS.
             10 WRK-OUT-OPCAO-RATM.
               15 WRK-OUT-OPCAO-RATM-N    PIC  9(001)      VALUE ZEROS.
             10 WRK-OUT-DANOOPER-ATTR-RATM
                                          PIC  9(004) COMP VALUE ZEROS.
             10 WRK-OUT-DANOOPER-RATM.
               15 WRK-OUT-DANOOPER-RATM-N PIC  9(004)      VALUE ZEROS.
             10 WRK-OUT-NSEQOPER-ATTR-RATM
                                          PIC  9(004) COMP VALUE ZEROS.
             10 WRK-OUT-NSEQOPER-RATM.
               15 WRK-OUT-NSEQOPER-RATM-N PIC  9(009)      VALUE ZEROS.
             10 WRK-OUT-PARC-ATTR-RATM    PIC S9(004) COMP VALUE ZEROS.
             10 WRK-OUT-PARC-RATM.
               15 WRK-OUT-PARC-RATM-N     PIC  9(005)      VALUE ZEROS.
             10 WRK-OUT-AGENCIA-ATTR-RATM PIC  9(004) COMP VALUE ZEROS.

             10 WRK-OUT-AGENCIA-RATM.
               15 WRK-OUT-AGENCIA-RATM-N  PIC  9(005)      VALUE ZEROS.
             10 WRK-OUT-CNPJ-ATTR-RATM    PIC  9(004) COMP VALUE ZEROS.
             10 WRK-OUT-CNPJ-RATM.
4S2511*        15 WRK-OUT-CNPJ-RATM-N     PIC  9(009)      VALUE ZEROS.
               15 WRK-OUT-CNPJ-RATM-N     PIC  9(009)      VALUE ZEROS.
4S2511         15 WRK-OUT-CNPJ-RATM-ST    PIC  X(009)      VALUE SPACES.
             10 WRK-OUT-FILIAL-ATTR-RATM  PIC  9(004) COMP VALUE ZEROS.
             10 WRK-OUT-FILIAL-RATM.
4S2511*        15 WRK-OUT-FILIAL-RATM-N   PIC  9(004)      VALUE ZEROS.
               15 WRK-OUT-FILIAL-RATM-N   PIC  9(004)      VALUE ZEROS.
4S2511         15 WRK-OUT-FILIAL-RATM-ST  PIC  X(04)       VALUE SPACES.
             10 WRK-OUT-CTRL-ATTR-RATM    PIC  9(004) COMP VALUE ZEROS.
             10 WRK-OUT-CTRL-RATM.
               15 WRK-OUT-CTRL-RATM-N     PIC  9(002)      VALUE ZEROS.
             10 WRK-OUT-AGEN2-ATTR-RATM   PIC  9(004) COMP VALUE ZEROS.
             10 WRK-OUT-AGEN2-RATM.
               15 WRK-OUT-AGEN2-RATM-N    PIC  9(005)      VALUE ZEROS.
             10 WRK-OUT-CONTA-ATTR-RATM   PIC  9(004) COMP VALUE ZEROS.
             10 WRK-OUT-CONTA-RATM.
               15 WRK-OUT-CONTA-RATM-N    PIC  9(013)      VALUE ZEROS.
             10 WRK-OUT-MENSA-RATM        PIC  X(079)      VALUE SPACES.

      *---------------------------------------------------------------*
       01  FILLER                         PIC X(070)       VALUE

           '*** TELA DCOMRATM - BRAD0660 - SEG. 00 ***'.
      *---------------------------------------------------------------*

       01  WRK-660-DCOMRATM.
           05  660-LL-AREA-RATM        PIC  9(04)  COMP  VALUE 0034.
           05  660-LL-MENSAGEM-RATM    PIC  9(04)  COMP  VALUE 0294.
           05  660-SENHA-RATM          PIC  9(04)  COMP  VALUE 0037.
           05  660-COMANDO-RATM        PIC  9(04)  COMP  VALUE 2070.
           05  660-COPCAO-RATM         PIC  9(04)  COMP  VALUE 0001.
           05  660-TIMETP-RATM         PIC  9(04)  COMP  VALUE 0026.
           05  660-OPCAO-RATM          PIC  9(04)  COMP  VALUE 2003.
           05  660-DANOPER-RATM        PIC  9(04)  COMP  VALUE 2006.
           05  660-NSEOPER-RATM        PIC  9(04)  COMP  VALUE 2011.
           05  660-PARC-RATM           PIC  9(04)  COMP  VALUE 2007.
           05  660-AGENCIA-RATM        PIC  9(04)  COMP  VALUE 2007.
           05  660-CNPJ-RATM           PIC  9(04)  COMP  VALUE 2011.
           05  660-FILIAL-RATM         PIC  9(04)  COMP  VALUE 2006.
           05  660-CTRL-RATM           PIC  9(04)  COMP  VALUE 2004.
           05  660-AGENC2-RATM         PIC  9(04)  COMP  VALUE 2007.
           05  660-CONTA-RATM          PIC  9(04)  COMP  VALUE 2015.
           05  660-MENSA-RATM          PIC  9(04)  COMP  VALUE 0079.


      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREAS DA BRAD2000 ***'.
      *----------------------------------------------------------------*
4S2511* AREA DE CHAMADA BRAD2000
       01  WRK-BRAD2000                PIC  X(08)          VALUE 
           'BRAD2000'.
4S2511* AREA DE DADOS PARA BRAD2000
       01  WRK-AREA-BRAD2000.
           05  WRK-CAD-CPFCGC.
               10  WRK-CAD-NUMERO     PIC  X(09)          VALUE SPACES.
               10  WRK-CAD-FILIAL     PIC  X(04)          VALUE SPACES.
       77  WRK-CAD-CONTROLE           PIC  9(02)          VALUE ZEROS.

4S2511*01  WRK-CGC-CPF-0110.
4S2511*    03  WRK-NUMERO-0110         PIC  9(009)         VALUE ZEROS.
4S2511*    03  WRK-FILIAL-0110         PIC  9(005)         VALUE ZEROS.
4S2511*
4S2511*01  WRK-CONTROLE-0110           PIC  X(002)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(060)         VALUE
           '*** AREA DE COMUNICACAO COM O MODULO - DCOM6437 ***'.
      *----------------------------------------------------------------*

       COPY 'I#DCOMXE'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(060)         VALUE

           '*** AREA DE COMUNICACAO COM O MODULO - DCOM4174 ***'.
      *----------------------------------------------------------------*

       COPY 'I#DCOMXJ'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** ACESSO AO MODULO DCOM6402 ***'.
      *----------------------------------------------------------------*

       01  WRK-6402-ENTRADA.
           05 WRK-6402E-COD-RETORNO    PIC  X(004)         VALUE SPACES.
           05 WRK-6402E-MSG-RETORNO    PIC  X(079)         VALUE SPACES.
           05 WRK-6402E-RESTART        PIC  9(005)         VALUE ZEROS.
           05 WRK-6402E-FLAG           PIC  X(001)         VALUE SPACES.
           05 WRK-6402E-CFUNC-BDSCO    PIC  9(009)         VALUE ZEROS.
           05 WRK-6402E-CTERM          PIC  X(008)         VALUE SPACES.
           05 WRK-6402E-TP-PROCESSO    PIC  X(001)         VALUE SPACES.
           05 WRK-6402E-CBCO           PIC  9(003)         VALUE ZEROS.
           05 WRK-6402E-CAG-BCRIA      PIC  9(005)         VALUE ZEROS.
           05 WRK-6402E-CCTA-BCRIA-CLI PIC  9(013)         VALUE ZEROS.


       01  WRK-6402-SAIDA.
           05 WRK-6402S-COD-RETORNO    PIC  9(004)         VALUE ZEROS.
           05 WRK-6402S-MSG-RETORNO    PIC  X(079)         VALUE SPACES.
           05 WRK-6402S-FLAG           PIC  X(001)         VALUE SPACES.
      *
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(060)         VALUE
           '*** AREA DE COMUNICACAO COM O MODULO - DCOM5335 ***'.
      *----------------------------------------------------------------*

       01  5335-ENTRADA.
           05 5335E-COD-RETORNO        PIC  X(004)         VALUE SPACES.
           05 5335E-MSG-RETORNO        PIC  X(079)         VALUE SPACES.
           05 5335E-RESTART            PIC  9(005)         VALUE ZEROS.
           05 5335E-FLAG               PIC  9(001)         VALUE ZEROS.
           05 5335E-CFUNC-BDSCO        PIC  9(009)         VALUE ZEROS.
           05 5335E-CTERM              PIC  X(008)         VALUE SPACES.
           05 5335E-OPERACAO.
              10 5335E-DANO-OPER-DESC  PIC  9(004)         VALUE ZEROS.
              10 5335E-NSEQ-OPER-DESC  PIC  9(009)         VALUE ZEROS.

           05 5335E-NPCELA-DESC-COML   PIC  9(005)         VALUE ZEROS.
           05 5335E-CELMTO-DESC-COML   PIC  9(003)         VALUE ZEROS.
           05 5335E-CEVNTO-DESC-COML   PIC  9(003)         VALUE ZEROS.
           05 5335E-ACAO               PIC  X(001)         VALUE SPACES.

       01  5335-SAIDA.
           05 5335S-COD-RETORNO        PIC  9(004)         VALUE ZEROS.
           05 5335S-MSG-RETORNO        PIC  X(079)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(060)         VALUE
           '*** AREA DE COMUNICACAO COM O MODULO - DCOM5448 ***'.
      *----------------------------------------------------------------*

       01  5448-ENTRADA-ROTEADOR.
           03  5448E-COD-RETORNO                   PIC  X(004).
           03  5448E-MSG-RETORNO                   PIC  X(079).
           03  5448E-RESTART                       PIC  9(005).
           03  5448E-FLAG                          PIC  X(001).
           03  5448E-CFUNC-BDSCO                   PIC  9(009).
           03  5448E-CTERM                         PIC  X(008).

           03  5448E-DANO-OPER-DESC                PIC  9(004).
           03  5448E-NSEQ-OPER-DESC                PIC  9(009).
           03  5448E-FLAG-FUNCAO                   PIC  X(001).
           03  5448E-AGENCIA-USUARIO               PIC  9(005).
           03  5448E-TIPO-BAIXA                    PIC  X(001).

       01  5448-SAIDA-ROTEADOR.
           03  5448S-HEADER.
               05  5448S-COD-RETORNO               PIC  X(004).
               05  5448S-MSG-RETORNO               PIC  X(079).
               05  5448S-RESTART                   PIC  9(005).
               05  5448S-FLAG                      PIC  X(001).
               05  FILLER                          PIC  X(006).

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(060)         VALUE
           '*** AREA DE MENSAGENS                           ***'.
      *----------------------------------------------------------------*

       COPY 'I#DCOMSY'.


      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA BRAD7100 ***'.
      *----------------------------------------------------------------*

       COPY 'I#BRAD7C'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** FIM DA WORKING STORAGE ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       01  LNK-IO-PCB.
           05  LNK-IO-LTERM            PIC  X(008).
           05  FILLER                  PIC  X(002).
           05  LNK-IO-STATUS           PIC  X(002).
           05  FILLER                  PIC  X(012).

           05  LNK-IO-MODNAME          PIC  X(008).

       01  LNK-ALT-PCB.
           05  LNK-ALT-LTERM           PIC  X(008).
           05  FILLER                  PIC  X(002).
           05  LNK-ALT-STATUS          PIC  X(002).
           05  FILLER                  PIC  X(012).
           05  LNK-ALT-MODNAME         PIC  X(008).

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

           ENTRY 'DLITCBL'             USING LNK-IO-PCB
                                             LNK-ALT-PCB.


           MOVE WRK-GU                 TO WRK-FUNCAO.

      ******************************************************************
      * EFETUA O PROCESSAMENTO DA TRANSACAO                            *

      ******************************************************************
      *----------------------------------------------------------------*
       0000-PROCESSAR-TRANSACAO        SECTION.
      *----------------------------------------------------------------*

           CALL 'POOL5005'             USING WRK-VERSAO
                                             WRK-FUNCAO
                                             LNK-IO-PCB
                                             LNK-ALT-PCB
                                             WRK-MENSAGEM
                                             WRK-TELA
                                             WRK-COD-USER
                                             WRK-COD-DEPTO.

           IF  RETURN-CODE             EQUAL 04 OR 12
               GOBACK
           END-IF.

           MOVE SPACES                 TO WRK-OUT-DADOS-RATM.

           PERFORM 1000-ROTINA-PRINCIPAL.


           GO TO 0000-PROCESSAR-TRANSACAO.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * ROTINA PRINCIPAL DO PROGRAMA.                                  *
      ******************************************************************
      *----------------------------------------------------------------*
       1000-ROTINA-PRINCIPAL           SECTION.
      *----------------------------------------------------------------*

           MOVE 'DCOMRATM'             TO WRK-TELA.

           IF  WRK-TRANSACAO           NOT EQUAL 'DCOM1801'
               PERFORM 1100-RECEBER-CONTROLE
           ELSE
               PERFORM 1200-PROCESSAR-DCOMRATM
           END-IF.


           IF  WRK-TELA                EQUAL 'DCOMRATM'
               PERFORM 1300-PROCESSAR-BRAD0660
           END-IF.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * RECEBE CONTROLE DO PROGRAMA CHAMADOR - DCOM1501                *
      ******************************************************************
      *----------------------------------------------------------------*
       1100-RECEBER-CONTROLE           SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-MENSAGEM           TO WRK-COMU-AREA-15UN

           IF  WRK-TRANSACAO           EQUAL 'DCOM1808'
               IF  WRK-COMU-MENSA-15UN NOT EQUAL SPACES
                   MOVE WRK-COMU-TPMANUT-15UN

                                       TO WRK-OUT-OPCAO-RATM
                   MOVE WRK-COMU-AGENC-15UN
                                       TO WRK-OUT-AGENCIA-RATM-N
                   MOVE WRK-49369      TO WRK-OUT-AGENCIA-ATTR-RATM
                   MOVE WRK-COMU-MENSA-15UN
                                       TO WRK-OUT-MENSA-RATM
               ELSE
                   MOVE 'ENTRE COM UMA DAS COMBINACOES: OPERACAO OU AGEN
      -                 'CIA OU AGENCIA/CONTA OU CNPJ/CPF'
                                       TO WRK-OUT-MENSA-RATM
               END-IF
               GO TO 1100-99-FIM
           END-IF.

           IF  WRK-TRANSACAO           EQUAL 'DCOM1809'
               IF  WRK-COMU-MENSA-15UN NOT EQUAL SPACES
                 IF  WRK-COMU-AGENC-15UN NOT EQUAL ZEROS
                 AND WRK-COMU-CONTA-15UN NOT EQUAL ZEROS
                   MOVE WRK-COMU-TPMANUT-15UN
                                       TO WRK-OUT-OPCAO-RATM
                   MOVE WRK-COMU-AGENC-15UN

                                       TO WRK-OUT-AGEN2-RATM-N
                   MOVE WRK-COMU-CONTA-15UN
                                       TO WRK-OUT-CONTA-RATM-N
                   MOVE WRK-49369      TO WRK-OUT-AGEN2-ATTR-RATM
                                          WRK-OUT-CONTA-ATTR-RATM
                 END-IF
4S2511*          IF  WRK-COMU-CNPJCPF-15UN NOT EQUAL ZEROS
4S2511*          AND WRK-COMU-FILIAL-15UN  NOT EQUAL ZEROS
4S2511           IF  WRK-COMU-CNPJCPF-15UN NOT EQUAL SPACES
4S2511           AND WRK-COMU-FILIAL-15UN  NOT EQUAL SPACES
                 AND WRK-COMU-CONTROL-15UN NOT EQUAL ZEROS
                   MOVE WRK-COMU-TPMANUT-15UN
                                       TO WRK-OUT-OPCAO-RATM
                   MOVE WRK-COMU-CNPJCPF-15UN
                                       TO WRK-OUT-CNPJ-RATM-N
                   MOVE WRK-COMU-FILIAL-15UN
                                       TO WRK-OUT-FILIAL-RATM-N
                   MOVE WRK-COMU-CONTROL-15UN
                                       TO WRK-OUT-CTRL-RATM-N
                   MOVE WRK-49369      TO WRK-OUT-CNPJ-ATTR-RATM
                                          WRK-OUT-FILIAL-ATTR-RATM
                                          WRK-OUT-CTRL-ATTR-RATM
                 END-IF

                   MOVE WRK-COMU-MENSA-15UN
                                       TO WRK-OUT-MENSA-RATM
                   GO TO 1100-99-FIM
               ELSE
                   MOVE 'ENTRE COM UMA DAS COMBINACOES: OPERACAO OU AGEN
      -                 'CIA OU AGENCIA/CONTA OU CNPJ/CPF'
                                       TO WRK-OUT-MENSA-RATM
               END-IF
               GO TO 1100-99-FIM
           END-IF.

      *--> ENTRADA POR OPERACAO
           IF  WRK-COMU-MENSA-15UN      NOT EQUAL SPACES
               MOVE WRK-COMU-MENSA-15UN TO WRK-OUT-MENSA-RATM
           ELSE
               MOVE 'ENTRE COM UMA DAS COMBINACOES: OPERACAO OU AGENCIA
      -           'OU AGENCIA/CONTA OU CNPJ/CPF'
                                        TO WRK-OUT-MENSA-RATM
           END-IF.

      *----------------------------------------------------------------*

       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * PROCESSAMENTO PRINCIPAL                                        *
      ******************************************************************
      *----------------------------------------------------------------*
       1200-PROCESSAR-DCOMRATM         SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-MENSAGEM               TO WRK-INPUT-ENTRADA.
           MOVE WRK-INPUT-AUX              TO WRK-INPUT-RATM.

           PERFORM 1210-DEVOLVE-TELA

           EVALUATE TRUE
             WHEN  (WRK-INP-PFK-RATM       EQUAL '.'       AND
                    WRK-INP-COMANDO-RATM   EQUAL 'PFK01')  OR
                    WRK-INP-PFK-RATM       EQUAL 'H'
                    MOVE WRK-INP-MENSA-RATM
                                           TO WRK-OUT-MENSA-RATM


             WHEN  (WRK-INP-PFK-RATM       EQUAL '.'       AND
                    WRK-INP-COMANDO-RATM   EQUAL 'PFK03')  OR
                    WRK-INP-PFK-RATM       EQUAL '3'
                    PERFORM 1220-RETORNAR-CHAMADOR

             WHEN  (WRK-INP-PFK-RATM       EQUAL '.'       AND
                    WRK-INP-COMANDO-RATM   EQUAL 'PFK05')  OR
                    WRK-INP-PFK-RATM       EQUAL '5'
                    PERFORM 1225-MENU-ROTINAS

             WHEN  (WRK-INP-PFK-RATM       EQUAL '.'       AND
                    WRK-INP-COMANDO-RATM   EQUAL 'PFK10')  OR
                    WRK-INP-PFK-RATM       EQUAL 'A'
                    PERFORM 1230-MENU-DCOM

             WHEN   WRK-INP-PFK-RATM       EQUAL '.'       AND
                    WRK-INP-COMANDO-RATM   EQUAL SPACES
                    PERFORM 1240-TRATAR-ENTER-FASE1

             WHEN   OTHER

                    MOVE SG-MENSAGEM(38)   TO WRK-OUT-MENSA-RATM

           END-EVALUATE.


      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * DEVOLVER DADOS PARA A TELA                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       1210-DEVOLVE-TELA               SECTION.
      *----------------------------------------------------------------*

           IF  WRK-INP-OPCAO-RATM           NOT EQUAL SPACES
               INSPECT WRK-INP-OPCAO-RATM   REPLACING LEADING ' ' BY '0'
           END-IF.

           IF  WRK-INP-DANOOPER-RATM        NOT EQUAL SPACES

               INSPECT WRK-INP-DANOOPER-RATM
                                            REPLACING LEADING ' ' BY '0'
           END-IF.

           IF  WRK-INP-NSEQOPER-RATM        NOT EQUAL SPACES
               INSPECT WRK-INP-NSEQOPER-RATM
                                            REPLACING LEADING ' ' BY '0'
           END-IF.

           IF  WRK-INP-PARC-RATM            NOT EQUAL SPACES
               INSPECT WRK-INP-PARC-RATM
                                            REPLACING LEADING ' ' BY '0'
           END-IF.

           IF  WRK-INP-AGENCIA-RATM         NOT EQUAL SPACES
               INSPECT WRK-INP-AGENCIA-RATM REPLACING LEADING ' ' BY '0'
           END-IF.

4S2511*   XIF  WRK-INP-CNPJ-RATM            NOT EQUAL SPACES
4S2511*       XINSPECT WRK-INP-CNPJ-RATM    REPLACING LEADING ' ' BY '0'
4S2511*    END-IF.
4S2511*
4S2511*
4S2511*   XIF  WRK-INP-FILIAL-RATM          NOT EQUAL SPACES
4S2511*       XINSPECT WRK-INP-FILIAL-RATM  REPLACING LEADING ' ' BY '0'
4S2511*    END-IF.

           IF  WRK-INP-CTRL-RATM            NOT EQUAL SPACES
               INSPECT WRK-INP-CTRL-RATM    REPLACING LEADING ' ' BY '0'
           END-IF.

           IF  WRK-INP-AGEN2-RATM           NOT EQUAL SPACES
               INSPECT WRK-INP-AGEN2-RATM   REPLACING LEADING ' ' BY '0'
           END-IF.

           IF  WRK-INP-CONTA-RATM           NOT EQUAL SPACES
               INSPECT WRK-INP-CONTA-RATM   REPLACING LEADING ' ' BY '0'
           END-IF.

           MOVE WRK-INP-COPCAO-RATM    TO WRK-OUT-COPCAO-RATM.
           MOVE WRK-INP-PARC-RATM      TO WRK-OUT-PARC-RATM.
           MOVE WRK-INP-TIMETP-RATM    TO WRK-OUT-TIMETP-RATM.
           MOVE WRK-INP-OPCAO-RATM     TO WRK-OUT-OPCAO-RATM.

           MOVE WRK-INP-DANOOPER-RATM  TO WRK-OUT-DANOOPER-RATM.
           MOVE WRK-INP-NSEQOPER-RATM  TO WRK-OUT-NSEQOPER-RATM.
           MOVE WRK-INP-AGENCIA-RATM   TO WRK-OUT-AGENCIA-RATM.
           MOVE WRK-INP-CNPJ-RATM      TO WRK-OUT-CNPJ-RATM.
           MOVE WRK-INP-FILIAL-RATM    TO WRK-OUT-FILIAL-RATM.
           MOVE WRK-INP-CTRL-RATM      TO WRK-OUT-CTRL-RATM.
           MOVE WRK-INP-AGEN2-RATM     TO WRK-OUT-AGEN2-RATM.
           MOVE WRK-INP-CONTA-RATM     TO WRK-OUT-CONTA-RATM.

      *----------------------------------------------------------------*
       1210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * RETORNAR PARA O PROGRAMA CHAMADOR - DCOM1501                   *
      ******************************************************************
      *----------------------------------------------------------------*
       1220-RETORNAR-CHAMADOR          SECTION.
      *----------------------------------------------------------------*

           INITIALIZE COMU-AREA-PFK


           MOVE LENGTH OF COMU-AREA-PFK
                                       TO COMU-LL-PFK
           MOVE WRK-CHNG               TO WRK-FUNCAO
           MOVE 'DCOM1801'             TO WRK-TELA
                                          COMU-TRANSACAO-PFK
           MOVE 'A'                    TO COMU-PFK
           MOVE COMU-AREA-PFK          TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       1220-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1225-MENU-ROTINAS               SECTION.
      *----------------------------------------------------------------*

           INITIALIZE COMU-AREA-PFK

           MOVE LENGTH OF COMU-AREA-PFK
                                       TO COMU-LL-PFK

           MOVE WRK-CHNG               TO WRK-FUNCAO
           MOVE 'DCOM1801'             TO WRK-TELA
                                          COMU-TRANSACAO-PFK
           MOVE '5'                    TO COMU-PFK
           MOVE COMU-AREA-PFK          TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       1225-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1230-MENU-DCOM                  SECTION.
      *----------------------------------------------------------------*

           INITIALIZE COMU-AREA-PFK

           MOVE LENGTH OF COMU-AREA-PFK
                                       TO COMU-LL-PFK
           MOVE WRK-CHNG               TO WRK-FUNCAO
           MOVE 'DCOM1801'             TO WRK-TELA
                                          COMU-TRANSACAO-PFK

           MOVE 'A'                    TO COMU-PFK
           MOVE COMU-AREA-PFK          TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       1230-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * TRATAMENTOS DE FASE 1                                          *
      ******************************************************************
      *----------------------------------------------------------------*
       1240-TRATAR-ENTER-FASE1         SECTION.
      *----------------------------------------------------------------*

           PERFORM 1242-CONSISTENCIA-FISICA.

           PERFORM 1141-2-CHECAR-DISPONIBILIDADE .

           IF  WRK-INCONSIS        EQUAL SPACES
               PERFORM 1141-1-CHECAR-DISPONIBILIDADE
               IF  ROTSAI-6438-COD-RET NOT EQUAL ZEROS

                   MOVE ROTSAI-6438-MENS-RET
                                       TO WRK-OUT-MENSA-RATM
               ELSE
                  IF  WRK-INCONSIS            EQUAL SPACES
                      PERFORM 1250-PASSAR-CONTROLE
                  END-IF
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       1240-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESTA ROTINA CHECA A DISPONIBILIDADE DO SISTEMA.               *
      *----------------------------------------------------------------*
       1141-1-CHECAR-DISPONIBILIDADE   SECTION.
      *----------------------------------------------------------------*

           INITIALIZE ROTENT-6438-ENVIO.


           EVALUATE WRK-INP-OPCAO-RATM-N
             WHEN 1
               MOVE 'I'                TO ROTENT-6438-FUNC-ENV
             WHEN 2
               MOVE 'E'                TO ROTENT-6438-FUNC-ENV
             WHEN OTHER
               MOVE 'C'                TO ROTENT-6438-FUNC-ENV
           END-EVALUATE.

           MOVE 'N'                    TO ROTENT-6438-FLAG.
           MOVE WRK-COD-USER-N         TO ROTENT-6438-CFUNC-ENV.
           MOVE LNK-IO-LTERM           TO ROTENT-6438-CTERM.
           MOVE WRK-COD-DEPTO-N        TO ROTENT-6438-AGEUSU-ENV.
           MOVE ZEROS                  TO ROTENT-6438-PROD-ENV.
           MOVE 'OP'                   TO ROTENT-6438-MOT-ENV.
           MOVE 'M'                    TO ROTENT-6438-PROC-ENV
           MOVE 024                    TO ROTENT-6438-OPCAO-ENV
           MOVE 'DCOM6438'             TO WRK-MODULO.

           CALL WRK-MODULO             USING ROTENT-6438-ENVIO
                                             ROTSAI-6438-RETORNO

                                             ERRO-AREA
                                             WRK-SQLCA.

           IF  ROTSAI-6438-COD-RET     EQUAL '0099'
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.


      *----------------------------------------------------------------*
       1141-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1141-2-CHECAR-DISPONIBILIDADE   SECTION.
      *----------------------------------------------------------------*

           INITIALIZE ROTENT-5484-ENVIO
                      ROTSAI-5484-RETORNO.

           MOVE 'N'                    TO ROTENT-5484-FLAG.

           MOVE WRK-COD-USER-N         TO ROTENT-5484-CFUNC-ENV.
           MOVE LNK-IO-LTERM           TO ROTENT-5484-CTERM.
           MOVE 'DCOM5484'             TO WRK-MODULO.


           CALL WRK-MODULO             USING ROTENT-5484-ENVIO
                                             ROTSAI-5484-RETORNO
                                             ERRO-AREA
                                             WRK-SQLCA.


           IF  ROTSAI-5484-COD-RET     EQUAL 0099
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           ELSE
               IF  ROTSAI-5484-COD-RET NOT EQUAL ZEROS AND 0150 AND 0151
                                                                AND 0152
                   MOVE '*'            TO WRK-INCONSIS
                   MOVE ROTSAI-5484-MENS-RET TO WRK-OUT-MENSA-RATM
                   GO TO 1141-2-99-FIM
               END-IF

           END-IF.

      *----------------------------------------------------------------*
       1141-2-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * TRATAR CONSISTENCIA FISICA                                     *
      ******************************************************************
      *----------------------------------------------------------------*
       1242-CONSISTENCIA-FISICA        SECTION.
      *----------------------------------------------------------------*

           INITIALIZE ROTENT-6437-ENVIO
                      ROTSAI-6437-RETORNO.

           IF  WRK-INP-OPCAO-RATM      NOT NUMERIC OR
              (WRK-INP-OPCAO-RATM-N    NOT EQUAL 1 AND 2 AND 3)
               MOVE WRK-49369          TO WRK-OUT-OPCAO-ATTR-RATM
               MOVE '*'                TO WRK-INCONSIS
               MOVE SG-MENSAGEM(35)    TO WRK-OUT-MENSA-RATM

               GO TO 1242-99-FIM
           END-IF.

           IF  WRK-INP-DANOOPER-RATM   EQUAL SPACES AND
               WRK-INP-NSEQOPER-RATM   EQUAL SPACES AND
               WRK-INP-AGENCIA-RATM    EQUAL SPACES AND
               WRK-INP-CNPJ-RATM       EQUAL SPACES AND
               WRK-INP-FILIAL-RATM     EQUAL SPACES AND
               WRK-INP-CTRL-RATM       EQUAL SPACES AND
               WRK-INP-AGEN2-RATM      EQUAL SPACES AND
               WRK-INP-CONTA-RATM      EQUAL SPACES
               MOVE '*'                TO WRK-INCONSIS
               MOVE 'ENTRE COM UMA DAS COMBINACOES: OPERACAO OU AGENCIA
      -             'OU AGENCIA/CONTA OU CNPJ/CPF'
                                       TO  WRK-OUT-MENSA-RATM
               MOVE WRK-49345          TO WRK-OUT-DANOOPER-ATTR-RATM
               GO TO 1242-99-FIM
           END-IF

           IF  WRK-INP-DANOOPER-RATM    NOT EQUAL SPACES OR
               WRK-INP-NSEQOPER-RATM    NOT EQUAL SPACES

               ADD 1                    TO WRK-ACU-SEL
           END-IF.

           IF  WRK-INP-AGENCIA-RATM     NOT EQUAL SPACES
               ADD 1                    TO WRK-ACU-SEL
           END-IF.

           IF  WRK-INP-CNPJ-RATM         NOT EQUAL SPACES
           OR  WRK-INP-FILIAL-RATM       NOT EQUAL SPACES
           OR  WRK-INP-CTRL-RATM         NOT EQUAL SPACES
               ADD 1                    TO WRK-ACU-SEL
           END-IF.

           IF  WRK-INP-AGEN2-RATM       NOT EQUAL SPACES OR
               WRK-INP-CONTA-RATM       NOT EQUAL SPACES
               ADD 1                    TO WRK-ACU-SEL
           END-IF.

           IF  WRK-ACU-SEL              GREATER 1
               MOVE WRK-49369           TO WRK-OUT-DANOOPER-ATTR-RATM
                                           WRK-OUT-NSEQOPER-ATTR-RATM

                                           WRK-OUT-AGENCIA-ATTR-RATM
                                           WRK-OUT-CNPJ-ATTR-RATM
                                           WRK-OUT-FILIAL-ATTR-RATM
                                           WRK-OUT-CTRL-ATTR-RATM
                                           WRK-OUT-AGEN2-ATTR-RATM
                                           WRK-OUT-CONTA-ATTR-RATM
               MOVE '*'                 TO WRK-INCONSIS
               MOVE 'ENTRE COM UMA DAS COMBINACOES: OPERACAO OU AGENCIA
      -             'OU AGENCIA/CONTA OU CNPJ/CPF'
                                        TO  WRK-OUT-MENSA-RATM
               GO TO 1242-99-FIM
           END-IF.

           IF (WRK-INP-DANOOPER-RATM    NOT EQUAL SPACES
           AND(WRK-INP-DANOOPER-RATM    NOT NUMERIC
           OR  WRK-INP-DANOOPER-RATM    NOT GREATER ZEROS))

           OR (WRK-INP-NSEQOPER-RATM    NOT EQUAL SPACES
           AND(WRK-INP-NSEQOPER-RATM    NOT NUMERIC
           OR  WRK-INP-NSEQOPER-RATM    NOT GREATER ZEROS))



           OR (WRK-INP-DANOOPER-RATM    NOT EQUAL SPACES
           AND WRK-INP-NSEQOPER-RATM    EQUAL SPACES)

           OR (WRK-INP-DANOOPER-RATM    EQUAL SPACES
           AND WRK-INP-NSEQOPER-RATM    NOT EQUAL SPACES)
               MOVE WRK-49369           TO WRK-OUT-DANOOPER-ATTR-RATM
                                           WRK-OUT-NSEQOPER-ATTR-RATM
               MOVE '*'                 TO WRK-INCONSIS
               MOVE 'CODIGO DA OPERACAO INVALIDA'
                                        TO WRK-OUT-MENSA-RATM
               GO TO 1242-99-FIM
           END-IF.

           IF  WRK-INP-DANOOPER-RATM NOT EQUAL SPACES AND
               WRK-INP-NSEQOPER-RATM NOT EQUAL SPACES
               IF (WRK-INP-PARC-RATM        NOT EQUAL SPACES
               AND(WRK-INP-PARC-RATM        NOT NUMERIC
               OR  WRK-INP-PARC-RATM        NOT GREATER ZEROS))
               MOVE WRK-49369           TO WRK-OUT-DANOOPER-ATTR-RATM
                                           WRK-OUT-NSEQOPER-ATTR-RATM

                                           WRK-OUT-PARC-ATTR-RATM
               MOVE '*'                 TO WRK-INCONSIS
               MOVE 'CODIGO DA OPERACAO INVALIDA'
                                        TO WRK-OUT-MENSA-RATM
               GO TO 1242-99-FIM
           END-IF.

           IF  WRK-INP-AGENCIA-RATM      NOT EQUAL SPACES
           AND(WRK-INP-AGENCIA-RATM      NOT NUMERIC
           OR  WRK-INP-AGENCIA-RATM      NOT GREATER ZEROS)
               MOVE WRK-49369            TO WRK-OUT-AGENCIA-ATTR-RATM
               MOVE '*'                  TO WRK-INCONSIS
               MOVE 'AGENCIA INVALIDA'   TO WRK-OUT-MENSA-RATM
               GO TO 1242-99-FIM
           END-IF.

           IF  WRK-INP-AGENCIA-RATM      NOT EQUAL SPACES
               MOVE WRK-INP-AGENCIA-RATM-N
                                         TO ROTENT-6437-AGEPESQ-ENV
               PERFORM 1243-CONSISTIR-AGENCIA
               IF  WRK-INCONSIS          EQUAL '*'

                   MOVE WRK-49369        TO WRK-OUT-AGENCIA-ATTR-RATM
                   GO TO 1242-99-FIM
               END-IF
           END-IF.

4S2511* AJUSTE DE CONSISTENCIA: considerar SPACES OU LOW-VALUES
           IF (WRK-INP-CNPJ-RATM         NOT EQUAL SPACES
           AND WRK-INP-CNPJ-RATM         NOT EQUAL LOW-VALUES
           AND(WRK-INP-CNPJ-RATM         NOT NUMERIC
           OR  WRK-INP-CNPJ-RATM         NOT GREATER ZEROS))
           OR (WRK-INP-FILIAL-RATM       NOT EQUAL SPACES
           AND WRK-INP-FILIAL-RATM       NOT EQUAL LOW-VALUES
           AND WRK-INP-FILIAL-RATM       NOT NUMERIC)
           OR (WRK-INP-CTRL-RATM         NOT EQUAL SPACES
           AND WRK-INP-CTRL-RATM         NOT EQUAL LOW-VALUES
           AND(WRK-INP-CTRL-RATM         NOT NUMERIC
           OR  WRK-INP-CTRL-RATM         NOT GREATER ZEROS))

           OR (WRK-INP-CNPJ-RATM         NOT EQUAL SPACES
           AND WRK-INP-CNPJ-RATM         NOT EQUAL LOW-VALUES
           AND (WRK-INP-CTRL-RATM        EQUAL SPACES
           OR  WRK-INP-CTRL-RATM         EQUAL LOW-VALUES))

           OR ((WRK-INP-CNPJ-RATM        EQUAL SPACES
           OR  WRK-INP-CNPJ-RATM         EQUAL LOW-VALUES)
           AND WRK-INP-CTRL-RATM         NOT EQUAL SPACES
           AND WRK-INP-CTRL-RATM         NOT EQUAL LOW-VALUES)
               MOVE WRK-49369            TO WRK-OUT-CNPJ-ATTR-RATM
                                            WRK-OUT-FILIAL-ATTR-RATM

                                            WRK-OUT-CTRL-ATTR-RATM
               MOVE '*'                  TO WRK-INCONSIS
               MOVE 'CNPJ/CPF INVALIDO'  TO WRK-OUT-MENSA-RATM
               GO TO 1242-99-FIM
           END-IF.

4S2511* EXECUTA CONSISTENCIA QUANDO ALGUM CAMPO FOI INFORMADO (NAO SPACES/LOW-VA
           IF (WRK-INP-CNPJ-RATM         NOT EQUAL SPACES
           AND WRK-INP-CNPJ-RATM         NOT EQUAL LOW-VALUES)
           OR (WRK-INP-FILIAL-RATM       NOT EQUAL SPACES
           AND WRK-INP-FILIAL-RATM       NOT EQUAL LOW-VALUES)
           OR (WRK-INP-CTRL-RATM         NOT EQUAL SPACES
           AND WRK-INP-CTRL-RATM         NOT EQUAL LOW-VALUES)
               PERFORM 1244-CONSISTIR-CNPJ
               IF  WRK-INCONSIS          EQUAL '*'
                   MOVE WRK-49369        TO WRK-OUT-CNPJ-ATTR-RATM
                                            WRK-OUT-FILIAL-ATTR-RATM
                                            WRK-OUT-CTRL-ATTR-RATM
                   GO TO 1242-99-FIM
               END-IF
           END-IF.

           IF (WRK-INP-CONTA-RATM        NOT EQUAL SPACES
           OR  WRK-INP-AGEN2-RATM        NOT EQUAL SPACES)
               IF  WRK-INP-AGEN2-RATM    NOT NUMERIC

               OR  WRK-INP-AGEN2-RATM    NOT GREATER ZEROS
                   MOVE WRK-49369        TO WRK-OUT-AGEN2-ATTR-RATM
                   MOVE 'AGENCIA INVALIDA'
                                         TO WRK-OUT-MENSA-RATM
                   MOVE '*'              TO WRK-INCONSIS
                   GO TO 1242-99-FIM
               ELSE
                   MOVE WRK-INP-AGEN2-RATM-N
                                         TO ROTENT-6437-AGEPESQ-ENV
                   PERFORM 1243-CONSISTIR-AGENCIA
                   INITIALIZE WRK-6402-ENTRADA
                   MOVE WRK-INP-AGEN2-RATM-N TO WRK-6402E-CAG-BCRIA
                   MOVE WRK-INP-CONTA-RATM-N TO WRK-6402E-CCTA-BCRIA-CLI
                   PERFORM 1245-ACESSAR-DCOM6402
                   IF  WRK-INCONSIS      EQUAL '*'
                       MOVE WRK-49369    TO WRK-OUT-AGEN2-ATTR-RATM
                       GO TO 1242-99-FIM
                   END-IF
               END-IF

               IF  WRK-INP-CONTA-RATM    NOT NUMERIC

               OR  WRK-INP-CONTA-RATM    NOT GREATER ZEROS
                   MOVE WRK-49369        TO WRK-OUT-CONTA-ATTR-RATM
                   MOVE 'CONTA INVALIDA' TO WRK-OUT-MENSA-RATM
                   MOVE '*'              TO WRK-INCONSIS
                   GO TO 1242-99-FIM
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       1242-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1243-CONSISTIR-AGENCIA          SECTION.
      *----------------------------------------------------------------*

      *--- VERIFICAR PERFIL DO USUARIO
           MOVE WRK-COD-USER-N          TO ROTENT-6437-CFUNC-ENV.
           MOVE WRK-COD-DEPTO-N         TO ROTENT-6437-AGEUSU-ENV.
           MOVE LNK-IO-LTERM            TO ROTENT-6437-CTERM.
           MOVE 'A'                     TO ROTENT-6437-FUNCAO-ENV.

           MOVE 'DCOM6437'              TO WRK-MODULO.

           CALL WRK-MODULO             USING ROTENT-6437-ENVIO
                                             ROTSAI-6437-RETORNO
                                             ERRO-AREA
                                             WRK-SQLCA.

           IF  ROTSAI-6437-COD-RET     NOT EQUAL ZEROS
           OR  ROTSAI-6437-RESUL-RET   GREATER 3
               MOVE '*'                TO WRK-INCONSIS
               IF  ROTSAI-6437-RESUL-RET  GREATER 3
                   MOVE 'USUARIO NAO AUTORIZADO'
                                       TO WRK-OUT-MENSA-RATM
               ELSE
                   MOVE ROTSAI-6437-MENS-RET
                                       TO WRK-OUT-MENSA-RATM
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       1243-99-FIM.                    EXIT.

      *----------------------------------------------------------------*

      ******************************************************************
      * CONSISTIR CNPJ                                                 *
      * CODIGO DE RETORNO 12 E PARA ACEITAR O CNPJ DO BRADESCO         *
      ******************************************************************
      *----------------------------------------------------------------*
       1244-CONSISTIR-CNPJ             SECTION.
      *----------------------------------------------------------------*
4S2511* ALTERACAO: Migracao de formatos e substituicao de validacao
4S2511*            BRAD0110 -> BRAD2000 conforme padrao do projeto.
4S2511*            Inclusao de movimentacao para campos -ST
4S2511*            (WRK-NUMERO-0110-ST X(09), WRK-FILIAL-0110-ST X(04)).

4S2511*    MOVE WRK-INP-CNPJ-RATM-N    TO WRK-NUMERO-0110.

4S2511*    MOVIMENTACOES PARA OS CAMPOS DA BRAD2000
           MOVE ZEROS                   TO WRK-CAD-CONTROLE.
           MOVE WRK-INP-CNPJ-RATM       TO WRK-CAD-NUMERO.

           IF  WRK-INP-FILIAL-RATM-N   NOT EQUAL SPACES
           OR  WRK-INP-FILIAL-RATM-N   NOT EQUAL LOW-VALUES
4S2511*       XMOVE WRK-INP-FILIAL-RATM-N TO WRK-FILIAL-0110
               MOVE WRK-INP-FILIAL-RATM    TO WRK-CAD-FILIAL
           ELSE
4S2511*       XMOVE ZEROS              TO WRK-FILIAL-0110
               MOVE SPACES             TO WRK-CAD-FILIAL
           END-IF.

4S2511*   XMOVE SPACES                 TO WRK-CONTROLE-0110.

4S2511* SUBSTITUICAO: Utilizar BRAD2000 no lugar de BRAD0110
           CALL  WRK-BRAD2000          USING WRK-CAD-CPFCGC
                                             WRK-CAD-CONTROLE.

4S2511*    VALIDACAO DO RETORNO DA CHAMADA (ACEITA RETORNO 12)
           IF (RETURN-CODE             NOT EQUAL ZEROS
           AND RETURN-CODE             NOT EQUAL 12)
               MOVE '*'                TO WRK-INCONSIS
               MOVE 'CNPJ/CPF INVALIDO'TO WRK-OUT-MENSA-RATM
           ELSE
4S2511*    VALIDA SE O CODIGO INFORMADO AO PROGRAMA E IGUAL AO CALCULADO
               IF WRK-INP-CTRL-RATM-N     NOT EQUAL WRK-CAD-CONTROLE
                   MOVE '*'            TO WRK-INCONSIS
                   MOVE 'DIGITO DO CNPJ/CPF NAO CONFERE'
                                       TO WRK-OUT-MENSA-RATM
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       1244-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * CONSISTIR CONTA CORRENTE ENCERRADA, CANCELADA E TRAGUEADA      *
      *----------------------------------------------------------------*
       1245-ACESSAR-DCOM6402           SECTION.

      *----------------------------------------------------------------*

           INITIALIZE WRK-6402-SAIDA
                      ERRO-AREA.

           MOVE SPACES                 TO WRK-SQLCA.
           MOVE 237                    TO WRK-6402E-CBCO.
           MOVE 'O'                    TO WRK-6402E-TP-PROCESSO.
           MOVE 'DCOM6402'             TO WRK-MODULO.

           CALL WRK-MODULO             USING WRK-6402-ENTRADA
                                             WRK-6402-SAIDA
                                             ERRO-AREA
                                             WRK-SQLCA.

           IF  WRK-6402S-COD-RETORNO   NOT EQUAL ZEROS
               MOVE '*'                TO WRK-INCONSIS
               MOVE WRK-6402S-MSG-RETORNO
                                       TO WRK-OUT-MENSA-RATM
           END-IF.


      *----------------------------------------------------------------*
       1245-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       1250-PASSAR-CONTROLE           SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WRK-COMU-AREA-15UN.

           IF  WRK-INP-AGENCIA-RATM   NOT EQUAL SPACES
               MOVE 237               TO WRK-COMU-BANCO-15UN
               MOVE WRK-INP-AGENCIA-RATM-N
                                      TO WRK-COMU-AGENC-15UN
               MOVE 'DCOM1808'        TO WRK-TELA
           END-IF

           IF  WRK-INP-AGEN2-RATM NOT EQUAL SPACES
               MOVE 237           TO WRK-COMU-BANCO-15UN
               MOVE WRK-INP-AGEN2-RATM-N
                                  TO WRK-COMU-AGENC-15UN

               MOVE WRK-INP-CONTA-RATM-N
                                  TO WRK-COMU-CONTA-15UN
               MOVE 'DCOM1809'    TO WRK-TELA
           ELSE
              IF WRK-INP-CNPJ-RATM-N  NOT EQUAL SPACES
                 MOVE WRK-INP-CNPJ-RATM-N
                                   TO WRK-COMU-CNPJCPF-15UN
                 MOVE WRK-INP-FILIAL-RATM-N
                                   TO WRK-COMU-FILIAL-15UN
                 MOVE WRK-INP-CTRL-RATM-N
                                   TO WRK-COMU-CONTROL-15UN
                 MOVE 'DCOM1809'   TO WRK-TELA
              END-IF
           END-IF

           IF  WRK-INP-DANOOPER-RATM  NOT EQUAL SPACES
               MOVE WRK-INP-DANOOPER-RATM-N
                                      TO WRK-COMU-DANOOPER-15UN
               MOVE WRK-INP-NSEQOPER-RATM-N
                                      TO WRK-COMU-NSEQOPER-15UN
               MOVE WRK-INP-PARC-RATM TO WRK-COMU-PARCELA-15UN

               PERFORM 1253-ACESSAR-DCOM4174
               IF  WRK-INCONSIS       EQUAL '*'
                   MOVE WRK-49369     TO WRK-OUT-DANOOPER-ATTR-RATM
                                         WRK-OUT-NSEQOPER-ATTR-RATM
                   GO TO 1250-99-FIM
               END-IF

               INITIALIZE WRK-6402-ENTRADA
               MOVE 4174-SAI-CAG-BCRIA TO WRK-6402E-CAG-BCRIA
               MOVE 4174-SAI-CCTA-BCRIA-CLI
                                       TO WRK-6402E-CCTA-BCRIA-CLI
               PERFORM 1245-ACESSAR-DCOM6402
               IF  WRK-INCONSIS     NOT EQUAL SPACES
                   MOVE WRK-49369   TO WRK-OUT-DANOOPER-ATTR-RATM
                                       WRK-OUT-NSEQOPER-ATTR-RATM
               END-IF

               IF (WRK-INP-OPCAO-RATM-N EQUAL 1 OR 2) AND
                   WRK-INP-PARC-RATM    NOT EQUAL SPACES
                   PERFORM 1254-ACESSAR-DCOM5335


                   IF  WRK-INCONSIS     EQUAL '*'
                       MOVE WRK-49369   TO WRK-OUT-DANOOPER-ATTR-RATM
                                           WRK-OUT-NSEQOPER-ATTR-RATM
                                           WRK-OUT-PARC-ATTR-RATM
                       GO TO 1250-99-FIM
                   END-IF
               END-IF

               PERFORM 1252-TRATAR-OPERACAO
           END-IF.

           PERFORM 1251-MONTAR-COMU-AREA.

      *----------------------------------------------------------------*
       1250-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * PASSAR O CONTROLE PARA O PROGRAMA DCOM****                     *
      ******************************************************************
      *----------------------------------------------------------------*

       1251-MONTAR-COMU-AREA           SECTION.
      *----------------------------------------------------------------*

           MOVE LENGTH OF WRK-COMU-AREA-15UN
                                       TO WRK-COMU-LL-15UN

           MOVE WRK-INP-OPCAO-RATM-N   TO WRK-COMU-TPMANUT-15UN
           MOVE WRK-INP-SENHAS-RATM    TO WRK-COMU-SENHAS-15UN
           MOVE WRK-INP-TIMETP-RATM    TO WRK-COMU-TIMESTAMP-15UN.

           MOVE WRK-CHNG               TO WRK-FUNCAO
           MOVE 'DCOM1801'             TO WRK-COMU-TRANCODE-15UN
           MOVE WRK-COMU-AREA-15UN     TO WRK-MENSAGEM.


      *----------------------------------------------------------------*
       1251-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1252-TRATAR-OPERACAO            SECTION.

      *----------------------------------------------------------------*

           EVALUATE TRUE

               WHEN WRK-INP-OPCAO-RATM EQUAL '1'

                    IF WRK-INP-PARC-RATM  NOT EQUAL SPACES
                       MOVE 'DCOM1803'    TO WRK-TELA
                    ELSE
                       MOVE 'DCOM1802'    TO WRK-TELA
                    END-IF

               WHEN WRK-INP-OPCAO-RATM EQUAL '2'

                    IF WRK-INP-PARC-RATM NOT EQUAL SPACES
                       MOVE 'DCOM1805'    TO WRK-TELA
                    ELSE
                       MOVE 'DCOM1804'    TO WRK-TELA
                    END-IF

               WHEN WRK-INP-OPCAO-RATM EQUAL '3'


                    IF WRK-INP-PARC-RATM NOT EQUAL SPACES
                       MOVE 'DCOM1807'    TO WRK-TELA
                    ELSE
                       MOVE 'DCOM1806'    TO WRK-TELA
                    END-IF

           END-EVALUATE.

      *----------------------------------------------------------------*
       1252-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1253-ACESSAR-DCOM4174           SECTION.
      *----------------------------------------------------------------*

           INITIALIZE 4174-ENTRADA-ROTEADOR
                      4174-SAIDA-ROTEADOR.

           MOVE WRK-COD-USER-N         TO 4174-ENT-CFUNC-BDSCO

           MOVE LNK-IO-LTERM           TO 4174-ENT-CTERM
           MOVE WRK-INP-DANOOPER-RATM-N TO 4174-ENT-DANO-OPER-DESC
           MOVE WRK-INP-NSEQOPER-RATM-N TO 4174-ENT-NSEQ-OPER-DESC

           MOVE 'DCOM4174'             TO WRK-MODULO.

           CALL WRK-MODULO             USING 4174-ENTRADA-ROTEADOR
                                             4174-SAIDA-ROTEADOR
                                             ERRO-AREA
                                             WRK-SQLCA.

           IF  4174-SAI-COD-RETORNO     NOT EQUAL '0000'
               IF  4174-SAI-COD-RETORNO EQUAL '0003'
                   MOVE 'OPERACAO NAO PERMITE OPCAO SELECIONADA. OPERACA
      -                 'O INEXISTENTE' TO WRK-OUT-MENSA-RATM
                   MOVE '*'             TO WRK-INCONSIS
               ELSE
                   MOVE 4174-SAI-MSG-RETORNO
                                       TO ERR-TEXTO
                   IF  4174-SAI-COD-RETORNO EQUAL '0099'
                       MOVE 'DB2'      TO ERR-TIPO-ACESSO

                   ELSE
                       MOVE 'APL'      TO ERR-TIPO-ACESSO
                   END-IF
                   PERFORM 9999-PROCESSAR-ROTINA-ERRO
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       1253-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1254-ACESSAR-DCOM5335           SECTION.
      *----------------------------------------------------------------*

           INITIALIZE 5335-ENTRADA
                      5335-SAIDA.

           MOVE WRK-INP-DANOOPER-RATM-N TO 5335E-DANO-OPER-DESC
           MOVE WRK-INP-NSEQOPER-RATM-N TO 5335E-NSEQ-OPER-DESC
           MOVE WRK-INP-PARC-RATM       TO 5335E-NPCELA-DESC-COML.

           MOVE 007                     TO 5335E-CELMTO-DESC-COML.
           MOVE 008                     TO 5335E-CEVNTO-DESC-COML

           IF WRK-INP-OPCAO-RATM-N      EQUAL 1
              MOVE 'I'                  TO 5335E-ACAO
           ELSE
              MOVE 'E'                  TO 5335E-ACAO
           END-IF.

           MOVE 'DCOM5335'              TO WRK-MODULO.

           CALL WRK-MODULO              USING 5335-ENTRADA
                                              5335-SAIDA
                                              ERRO-AREA
                                              WRK-SQLCA.

           EVALUATE  5335S-COD-RETORNO
               WHEN '0000'
                     CONTINUE

               WHEN '0099'

                     MOVE 'DB2'        TO ERR-TIPO-ACESSO
                     PERFORM 9999-PROCESSAR-ROTINA-ERRO

               WHEN  OTHER
                     MOVE 5335S-MSG-RETORNO
                                       TO WRK-OUT-MENSA-RATM
                     MOVE '*'          TO WRK-INCONSIS
           END-EVALUATE.

      *----------------------------------------------------------------*
       1254-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * PROCESSA BRAD660 COMPACTA DADOS PARA DCOMRATM                  *
      ******************************************************************
      *----------------------------------------------------------------*
       1300-PROCESSAR-BRAD0660         SECTION.
      *----------------------------------------------------------------*

           CALL 'BRAD0660'             USING WRK-OUTPUT-RATM

                                             WRK-660-DCOMRATM.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE RETURN-CODE        TO WRK-COD-RET
               MOVE WRK-COD-RET-N      TO WRK-RETURN-CODE
               MOVE 010                TO WRK-LOCAL-ERRO
               MOVE WRK-ERRO-BRAD0660  TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE WRK-ISRT               TO WRK-FUNCAO.
           MOVE WRK-OUTPUT-RATM        TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      ******************************************************************
      * CHAMADA A BRAD7100 - TRATAMENTO DE ERRO                        *
      ******************************************************************

      *----------------------------------------------------------------*
       9999-PROCESSAR-ROTINA-ERRO      SECTION.
      *----------------------------------------------------------------*

           MOVE 'DCOM1801'             TO ERR-PGM.

           IF  ERR-MODULO              EQUAL SPACES OR LOW-VALUES
               MOVE 'DCOM1801'         TO ERR-MODULO
           END-IF.

           MOVE  WRK-COD-USER          TO ERR-COD-USER.
           MOVE  WRK-COD-DEPTO         TO ERR-COD-DEPTO.

           CALL 'BRAD7100'             USING LNK-IO-PCB
                                             ERRO-AREA
                                             LNK-ALT-PCB
                                             WRK-SQLCA.

           GOBACK.

      *----------------------------------------------------------------*

       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
