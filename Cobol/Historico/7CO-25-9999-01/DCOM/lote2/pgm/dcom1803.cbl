
***************************** Top of Data ******************************
      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID.     DCOM1803.
       AUTHOR.         FABRICA - ALTRAN.
      *================================================================*
      *                          A L T R A N                           *
      *----------------------------------------------------------------*
      *     PROGRAMA....: DCOM1803                                     *
      *     PROGRAMADOR.: FABRICA - ALTRAN.                            *
      *     ANALISTA....: MARCOS TADEU.                                *
      *     DATA........: 17/04/2007                                   *
      *----------------------------------------------------------------*
      *     OBJETIVO....: INCLUIR PRORROGACAO DE VENCIMENTO - DETALHES *
      *----------------------------------------------------------------*
      *     TELAS.......:                                              *
      *     DCOMRCTM....: INCLUIR PRORROGACAO DE VENCIMENTO - DETALHES *
      *----------------------------------------------------------------*
      *     INC'S.......:                                              *

      *     I#BRAD7C - AREA DE ERRO DA BRAD7100                        *
      *     I#DCOMSG - TABELA DE MENSAGENS DE RETORNO DO               *
      *                SISTEMA DCOM (ON-LINE)                          *
      *----------------------------------------------------------------*
      *     MODULOS.....:                                              *
      *     BRAD0660 - INSERIR CARACTERES DE NULL NA MENSAGEM DE SAIDA *
      *     BRAD5010 - CONTROLE DO TRANSITO DE MENSAGENS NO IMS/DC     *
      *     BRAD7100 - TRATAMENTO DE ERROS QUANDO PROGRAMA INVALIDO    *
      *     BRAD7600  FORNECE DATA E HORA CORRENTE DO SISTEMA          *
      *----------------------------------------------------------------*
      *     NAVEGACAO...:                                              *
      *     CHAMADO POR.: DCOM1802/DCOM1801                            *
      *================================================================*
BSI001*                    ALTERACAO - BSI TECNOLOGIA                  *
BSI001*----------------------------------------------------------------*
BSI001*  ANALISTA BSI..:   LUCIANDRA SILVEIRA - BSI TECNOLOGIA         *
BSI001*  ANALISTA DDS..:   RICARDO JAMMAL     - BRADESCO - GRUPO 39    *
BSI001*  DATA..........:   30/04/2014                                  *
BSI001*----------------------------------------------------------------*
BSI001*  OBJETIVO......: - "BRILHAR" CAMPO TAXA QUANDO RETORNO INCON-  *
BSI001*                    SISTENTE DO MODULO DCOM5470.                *

BSI001*              ..: - ADEQUAR PROGRAMA AO HEXAVISION.             *
BSI001*                    PMO 14/0017 - PARAMETROS DCRU NO DCOM.      *
BSI001*================================================================*
BRQ001*                    ALTERACAO - BRQ                             *
BRQ001*----------------------------------------------------------------*
BRQ001*  ANALISTA BRQ..:   FABIO AUGUSTO FINK                          *
BRQ001*  ANALISTA DS...:   RICARDO JAMMAL     - BRADESCO - GRUPO 39    *
BRQ001*  DATA..........:   24/11/2017                                  *
BRQ001*----------------------------------------------------------------*
BRQ001*  OBJETIVO......: - INCLUIR CAMPO BASE LEGAL PRORROGACAO PARA   *
BRQ001*                    ADEQUAR FUNCIONALIDADE AO SICOR 4.10.       *
BRQ001*----------------------------------------------------------------*
BRQ001*  MODULO........:                                               *
BRQ001*         DCOM5218 - OBTER E CONSISTIR CODIGO BASE LEGAL PRORRO- *
BRQ001*                    GACAO BACEN.                                *
BRQ001*----------------------------------------------------------------*
BRQ001*  NAVEGACAO.....:                                               *
BRQ001*     CHAMADO POR:    ---            CHAMA: DCOM3929             *
BRQ001*================================================================*

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
       01  FILLER                      PIC  X(50)          VALUE
           '*** DCOM1803 - INICIO DA AREA DE WORKING ***'.

      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*** AREA DE ATRIBUTOS ***'.
      *----------------------------------------------------------------*

      *--- ALPHA    - NORMAL    - PROTEGIDO    - NAO POS CURSOR ---*
       77  WRK-225                     PIC S9(08) COMP     VALUE +241.
      *--- ALPHA    - NODISP    - PROTEGIDO    - NAO POS CURSOR ---*
       77  WRK-229                     PIC S9(08) COMP     VALUE +229.
      *--- NUMERICO - NORMAL    - PROTEGIDO    - NAO POS CURSOR ---*
       01  WRK-241                     PIC S9(08) COMP     VALUE +241.
      *--- NUMERIC  - NORMAL    - DESPROTEGIO  - NAO POS CURSOR ---*
       77  WRK-209                     PIC S9(08) COMP     VALUE +209.
      *--- NUMERICO - NORMAL    - PROTEGIDO    - POS. CURSOR    ---*
       01  WRK-49361                   PIC S9(08) COMP     VALUE +49361.
      *--- NUMERICO - BRILHANTE - DESPROTEGIDO - POS. CURSOR    ---*
       01  WRK-49369                   PIC S9(08) COMP     VALUE +49369.
      *
       01  WRK-UNNS-C                      PIC S9(08) COMP VALUE +49361.

       01  WRK-PNNS                        PIC S9(08) COMP VALUE +225.
       01  WRK-PNNS-C                      PIC S9(08) COMP VALUE +49377.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*** VARIAVEIS AUXILIARES ***'.
      *----------------------------------------------------------------*

       01  WRK-AUXILIARES.
           05  WRK-MODULO              PIC  X(08)          VALUE SPACES.
           05  WRK-INCONSIS            PIC  X(01)          VALUE SPACES.
           05  WRK-FLAG-CONSIST        PIC  X(01)          VALUE SPACES.

           05  WRK-DT-VENCTO           PIC  9(08)          VALUE ZEROS.
           05  WRK-DT-VENCTO-X         REDEFINES WRK-DT-VENCTO.
               10  WRK-AA-SAMD-VCTO    PIC  9(04).
               10  WRK-MM-SAMD-VCTO    PIC  9(02).
               10  WRK-DD-SAMD-VCTO    PIC  9(02).

           05  WRK-DT-SAMD-N           PIC  9(08)          VALUE ZEROS.
           05  WRK-DT-SAMD             REDEFINES WRK-DT-SAMD-N.

               10  WRK-AA-SAMD         PIC  9(04).
               10  WRK-MM-SAMD         PIC  9(02).
               10  WRK-DD-SAMD         PIC  9(02).

           05  WRK-DT-AUX.
               10  WRK-DT-AUX-DD       PIC  X(02)          VALUE SPACES.
               10  FILLER              PIC  X(01)          VALUE '/'.
               10  WRK-DT-AUX-MM       PIC  X(02)          VALUE SPACES.
               10  FILLER              PIC  X(01)          VALUE '/'.
               10  WRK-DT-AUX-AA       PIC  X(04)          VALUE SPACES.

           05  WRK-BCOAGECTA-AUX.
               10  WRK-BANCO-AUX       PIC  9(03)          VALUE ZEROS.
               10  FILLER              PIC  X(01)          VALUE '/'.
               10  WRK-AGENC-AUX       PIC  9(05)          VALUE ZEROS.
               10  FILLER              PIC  X(01)          VALUE '/'.
               10  WRK-CONTA-AUX       PIC  9(13)          VALUE ZEROS.

           05  CAMPO3                  PIC  X(013)    VALUE SPACES.
           05  CAMPO4                  PIC  9(009)    VALUE ZEROS.


           05  WRK-TXJURA-4174         PIC  9(003)V9(005)  VALUE ZEROS.
           05  WRK-TXJURM-4174         PIC  9(003)V9(005)  VALUE ZEROS.
           05  WRK-TXJURM              PIC  9(003)V9(005)  VALUE ZEROS.

           05  WRK-TAXA-JURO-MES       PIC  9(003)V9(005)  VALUE ZEROS.
           05  WRK-TAXA-JURO-MIN       PIC  9(002)V9(003)  VALUE ZEROS.
           05  WRK-TAXA-JURO-MAX       PIC  9(002)V9(003)  VALUE ZEROS.

           05  WRK-TIMESTAMP           PIC  X(26)          VALUE SPACES.

           05  WRK-TAXA-MINMAX.
               10 FILLER               PIC  X(06)          VALUE
                  '(MIN: '.
               10 WRK-TAX-MIN          PIC  99,999         VALUE ZEROS.
               10 FILLER               PIC  X(06)          VALUE
                  ' MAX: '.
               10 WRK-TAX-MAX          PIC  99,999         VALUE ZEROS.
               10 FILLER               PIC  X(01)          VALUE ')'.

      *--> TAXA DE JUROS MENSAL
           05  WRK-AREA-BRAD0496.

               10  WRK-VALOR-0496      PIC  X(009)         VALUE SPACES.
               10  WRK-SAIDA-0496.
                   15 WRK-SAIDA-0496-N PIC  9(003)V9(005)  VALUE ZEROS.
               10  WRK-TAMANHO-0496    PIC  9(008)         VALUE ZEROS.

      *--> TAXA DE JUROS MIN / MAX
           05  WRK-AREA-BRAD0496-2.
               10  WRK-VALOR-0496-2    PIC  X(006)         VALUE SPACES.
               10  WRK-SAIDA-0496-2.
                   15 WRK-SAIDA-0496-2-N
                                       PIC  9(02)V9(03)  VALUE ZEROS.
               10  WRK-TAMANHO-0496-2  PIC  9(008)         VALUE ZEROS.

      *--> VALOR PRINCIPAL
           05  WRK-AREA-BRAD0496-3.
               10  WRK-VALOR-0496-3      PIC  X(017)       VALUE SPACES.
               10  WRK-SAIDA-0496-3.
                   15 WRK-SAIDA-0496-N-3 PIC  9(011)V9(002) VALUE ZEROS.
               10  WRK-TAMANHO-0496-3    PIC  9(008)        VALUE ZEROS.

      *--> VALOR JUROS

           05  WRK-AREA-BRAD0496-4.
               10  WRK-VALOR-0496-4      PIC  X(014)       VALUE SPACES.
               10  WRK-SAIDA-0496-4.
                   15 WRK-SAIDA-0496-N-4 PIC  9(009)V9(002) VALUE ZEROS.
               10  WRK-TAMANHO-0496-4    PIC  9(008)        VALUE ZEROS.


           05  WRK-MSG-4473-1          PIC  X(079)        VALUE
               'TAXA ABAIXO DA PERMITIDA PELO PRODUTO'.

           05  WRK-MSG-4473-2          PIC  X(079)         VALUE
               'TAXA ACIMA DA PERMITIDA PELO PRODUTO'.

           05  WRK-CHAVE.
BSI001         07  WRK-OPERACAO        PIC  9(013)         VALUE ZEROS.
               07  FILLER REDEFINES WRK-OPERACAO.
                   10  WRK-DANO-OPER-DESC-1      PIC  9(04).
                   10  WRK-NSEQ-OPER-DESC-1      PIC  9(09).
               07  WRK-FILLER                PIC  X(27) VALUE SPACES.

4S2511*    05  WRK-NRO-CNPJ            PIC  9(009)         VALUE ZEROS.
4S2511     05  WRK-NRO-CNPJ            PIC  X(009)         VALUE SPACES.

BSI001     05  FILLER REDEFINES        WRK-NRO-CNPJ.
4S2511         10 WRK-CNPJ-1           PIC  X(003).
4S2511         10 WRK-CNPJ-2           PIC  X(003).
4S2511         10 WRK-CNPJ-3           PIC  X(003).

       01  WRK-TAXA-1                   PIC  9(03)V9(05) VALUE ZEROS.
       01  WRK-TAXA-2                   PIC  9(03)V9(05) VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
          '*** AREA DE COMUNICACAO MODULO DCOM4170 ***'.
      *----------------------------------------------------------------*

       01     4170-ENTRADA.
         05   4170-ENT-COD-RET           PIC  X(04)        VALUE SPACES.
         05   4170-ENT-MSG-RET           PIC  X(79)        VALUE SPACES.
         05   4170-ENT-RESTART           PIC  9(05)        VALUE ZEROS.
         05   4170-ENT-FLAG              PIC  X(01)        VALUE ZEROS.
         05   4170-ENT-START-COUNT       PIC  X(01)        VALUE SPACES.
         05   4170-ENT-CFUNC-BDSCO       PIC  9(09)        VALUE ZEROS.
         05   4170-ENT-CTERM             PIC  X(08)        VALUE SPACES.

         05   4170-ENT-CCHAVE-ELMTO-DESC PIC  X(45)        VALUE SPACES.
         05   4170-ENT-CINDCD-PSSOA-DESC PIC  X(02)        VALUE SPACES.
         05   4170-ENT-CELMTO-DESC-COML  PIC  9(03)        VALUE ZEROS.
       01     4170-SAIDA.
         05   4170-SAI-COD-RET           PIC  X(004)       VALUE SPACES.
         05   4170-SAI-MSG-RET           PIC  X(079)       VALUE SPACES.
         05   4170-SAI-RESTART           PIC  9(005)       VALUE ZEROS.
         05   4170-SAI-FLAG              PIC  X(001)       VALUE ZEROS.
         05   4170-SAI-QTD-RETORNA       PIC  9(003)       VALUE ZEROS.
         05   4170-SAI-COUNT             PIC  9(009)       VALUE ZEROS.
         05   4170-SAI-DADOS.
4S2511*    10 4170-SAI-CCNPJ-CPF         PIC  9(009)       VALUE ZEROS.
4S2511     10 4170-SAI-CCNPJ-CPF         PIC  X(09)        VALUE SPACES.
4S2511*    10 4170-SAI-CFLIAL-CNPJ       PIC  9(005)       VALUE ZEROS.
4S2511     10 4170-SAI-CFLIAL-CNPJ       PIC  X(04)        VALUE SPACES.
4S2511*    10 4170-SAI-CCNPJ-CPF-ST      PIC  X(09)        VALUE SPACES.
4S2511*    10 4170-SAI-CFLIAL-CNPJ-ST    PIC  X(04)        VALUE SPACES.
           10 4170-SAI-CCTRL-CNPJ-CPF    PIC  9(002)       VALUE ZEROS.
           10 4170-SAI-CDOCTO-ID         PIC  X(015)       VALUE SPACES.
           10 4170-SAI-IPSSOA-DESC-COML  PIC  X(060)       VALUE SPACES.
           10 4170-SAI-CCLUB             PIC  9(010)       VALUE ZEROS.
           10 4170-SAI-CBCO              PIC  9(003)       VALUE ZEROS.
           10 4170-SAI-NOME-BANCO        PIC  X(040)       VALUE SPACES.
           10 4170-SAI-CAG-BCRIA         PIC  9(005)       VALUE ZEROS.
           10 4170-SAI-NOME-AGENCIA      PIC  X(040)       VALUE SPACES.

           10 4170-SAI-CCTA-BCRIA-CLI    PIC  9(013)       VALUE ZEROS.
           10 4170-SAI-CPOSTO-SERVC      PIC  9(003)       VALUE ZEROS.
           10 4170-SAI-IPSSOA-CONJG      PIC  X(040)       VALUE SPACES.
           10 4170-SAI-CCPF-CONJG        PIC  9(009)       VALUE ZEROS.
           10 4170-SAI-CCTRL-CPF-CONJG   PIC  9(002)       VALUE ZEROS.
           10 4170-SAI-CDOCTO-ID-CONJG   PIC  X(015)       VALUE SPACES.
           10 4170-SAI-CINDCD-CONJG-AUTRZ
                                         PIC  X(001)       VALUE SPACES.
           10 4170-SAI-CCEP              PIC  9(005)       VALUE ZEROS.
           10 4170-SAI-CCEP-COMPL        PIC  9(003)       VALUE ZEROS.
           10 4170-SAI-ELOGDR            PIC  X(040)       VALUE SPACES.
           10 4170-SAI-CSGL-UF           PIC  X(002)       VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
          '*** AREA DE COMUNICACAO MODULO DCOM5335 ***'.
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
       01  FILLER                      PIC  X(050)         VALUE
          '*** AREA DE COMUNICACAO MODULO DCOM7995 ***'.
      *----------------------------------------------------------------*


       01  WRK-ENTRADA-DCOM7995.
           05  WRK-ENT-OPCAO           PIC  X(02)          VALUE SPACES.
           05  WRK-ENT-COD-FUNC        PIC  9(09)          VALUE ZEROS.
           05  WRK-ENT-COD-TERMINAL    PIC  X(08)          VALUE SPACES.
           05  WRK-ENT-TIMESTAMP       PIC  X(26)          VALUE SPACES.
           05  WRK-ENT-TRANS           PIC  X(08)          VALUE SPACES.
           05  WRK-ENT-TRANS-ANT       PIC  X(08)          VALUE SPACES.
           05  WRK-ENT-PAGINA          PIC  9(03)          VALUE ZEROS.
           05  WRK-ENT-DADOS           PIC  X(1600)        VALUE SPACES.

       01  WRK-SAIDA-DCOM7995.
           05  WRK-SAI-COD-RETORNO     PIC  9(02)          VALUE ZEROS.
           05  WRK-SAI-MSG-RETORNO     PIC  X(79)          VALUE SPACES.
           05  WRK-SAI-TIMESTAMP       PIC  X(26)          VALUE SPACES.
           05  WRK-SAI-TRANS           PIC  X(08)          VALUE SPACES.
           05  WRK-SAI-TRANS-ANT       PIC  X(08)          VALUE SPACES.
           05  WRK-SAI-PAGINA          PIC  9(03)          VALUE ZEROS.
           05  WRK-SAI-DADOS           PIC  X(1600)        VALUE SPACES.


      *----------------------------------------------------------------*

       01  FILLER                      PIC  X(50)          VALUE
           '*** AREA DE COMUNICACAO DCOM1801 ***'.
      *----------------------------------------------------------------*

       01  WRK-COMU-AREA.
           05 WRK-COMU-LL              PIC S9(004) COMP    VALUE ZEROS.
           05 WRK-COMU-ZZ              PIC S9(004) COMP    VALUE ZEROS.
           05 WRK-COMU-TRANCODE.
             10 WRK-COMU-TRANSACAO     PIC  X(008)         VALUE SPACES.
             10 FILLER                 PIC  X(006)         VALUE SPACES.
             10 WRK-COMU-PFK           PIC  X(001)         VALUE SPACES.
           05 WRK-COMU-SENHAS          PIC  X(037)         VALUE SPACES.
           05 WRK-COMU-COMANDO         PIC  X(068)         VALUE SPACES.
           05 WRK-COMU-OPCAO           PIC  9(001)         VALUE ZEROS.
           05 WRK-COMU-TIMESTAMP       PIC  X(026)         VALUE SPACES.
           05 WRK-COMU-DADOS.
             10 WRK-COMU-OPERACAO      PIC  9(013)         VALUE ZEROS.
             10 FILLER REDEFINES       WRK-COMU-OPERACAO.
               15 WRK-COMU-DANO-OPER   PIC  9(004).
               15 WRK-COMU-NSEQ-OPER   PIC  9(009).
             10 WRK-COMU-PARCELA       PIC  9(005)         VALUE ZEROS.

             10 WRK-COMU-BCAGCT.
               15 WRK-COMU-BANCO       PIC  9(003)         VALUE ZEROS.
               15 WRK-COMU-AGENCIA     PIC  9(005)         VALUE ZEROS.
               15 WRK-COMU-CONTA       PIC  9(013)         VALUE ZEROS.
             10 WRK-COMU-CNPJCPF.
4S2511         15 WRK-COMU-CCNPJ-CPF   PIC  X(09)          VALUE SPACES.
4S2511         15 WRK-COMU-CFLIAL-CNPJ PIC  X(04)          VALUE SPACES.
               15 WRK-COMU-CCTRL-CNPJCPF
                                       PIC  9(002)         VALUE ZEROS.
             10 WRK-COMU-CPRODUTO      PIC  9(003)         VALUE ZEROS.
             10 WRK-COMU-IPRODUTO      PIC  X(025)         VALUE SPACES.
             10 WRK-COMU-CSUBPROD      PIC  9(003)         VALUE ZEROS.
             10 WRK-COMU-ISUBPROD      PIC  X(025)         VALUE SPACES.
             10 WRK-COMU-PAGINA-1      PIC  9(004)         VALUE ZEROS.
             10 WRK-COMU-PAGINA-2      PIC  9(004)         VALUE ZEROS.
             10 WRK-COMU-TP-SELECAO    PIC  9(001)         VALUE ZEROS.
      *-------- - 3 = SELECAO POR AGENCIA         (DCOM0903)
      *-------- - 4 = SELECAO POR AGENCIA E CONTA (DCOM0904)
      *-------- - 5 = SELECAO POR CNPJ/CPF        (DCOM0904)
             10 WRK-COMU-MENSAGEM      PIC  X(079)         VALUE SPACES.
             10 WRK-COMU-CHAMADOR      PIC  X(008)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(080)         VALUE
           '*** AREA DE COMUNICACAO PROGRAMA DCOM0105              ***'.
      *----------------------------------------------------------------*

       01  COMU-AREA-0105.
           05  COMU-LL-0105            PIC S9(004) COMP    VALUE +596.
           05  COMU-ZZ-0105            PIC S9(004) COMP    VALUE ZEROS.
           05  COMU-TRANCODE-0105.
               10  COMU-TRANSACAO-0105 PIC  X(008)         VALUE SPACES.
               10  FILLER              PIC  X(006)         VALUE SPACES.
               10  COMU-PFK-0105       PIC  X(001)         VALUE SPACES.
           05  COMU-SENHAS-0105        PIC  X(037)         VALUE SPACES.
           05  COMU-COMANDO-0105       PIC  X(068)         VALUE SPACES.
           05  COMU-DADOS-0105.
               10  COMU-TPOPCAO-0105   PIC  X(001)         VALUE SPACES.
               10  COMU-TMSTAMP-0105   PIC  X(026)         VALUE SPACES.
               10  COMU-NOME-0105      PIC  X(040)         VALUE SPACES.

               10  COMU-CNPJ-0105.
4S2511*            15  COMU-CCNPJ-0105 PIC  9(009)         VALUE ZEROS.
4S2511             15  COMU-CCNPJ-0105 PIC  X(09)          VALUE SPACES.

BSI001             15  FILLER          PIC  X(001)         VALUE SPACES.
4S2511*            15  COMU-CFLIAL-0105 PIC  9(004)        VALUE ZEROS.
4S2511             15  COMU-CFLIAL-0105 PIC  X(04)         VALUE SPACES.
BSI001             15  FILLER          PIC  X(001)         VALUE SPACES.
                   15  COMU-CTRL-0105  PIC  9(002)         VALUE ZEROS.
               10  COMU-BCOAGECTA-0105.
                   15  COMU-BANCO-0105 PIC  9(003)         VALUE ZEROS.
BSI001             15  FILLER          PIC  X(001)         VALUE SPACES.
                   15  COMU-AGENC-0105 PIC  9(005)         VALUE ZEROS.
BSI001             15  FILLER          PIC  X(001)         VALUE SPACES.
                   15  COMU-CONTA-0105 PIC  9(013)         VALUE ZEROS.
               10  COMU-CODPROD-0105   PIC  9(003)         VALUE ZEROS.
               10  COMU-DESPROD-0105   PIC  X(012)         VALUE SPACES.
               10  COMU-CODSUBP-0105   PIC  9(003)         VALUE ZEROS.
               10  COMU-DESSUBP-0105   PIC  X(012)         VALUE SPACES.
               10  COMU-OPERACAO-0105  PIC  9(013)         VALUE ZEROS.
               10  COMU-OPERACAO-0105-X  REDEFINES  COMU-OPERACAO-0105.
                   15  COMU-DANO-0105  PIC  9(004).
                   15  COMU-NSEQ-0105  PIC  9(009).
               10  COMU-SITUAC-0105    PIC  X(015)         VALUE SPACES.
               10  COMU-DESCIDEN-0105  PIC  X(013)         VALUE SPACES.
               10  COMU-NUMERO-0105.

                   15  COMU-NUMERO-0105-N   PIC  9(009)    VALUE ZEROS.
               10  COMU-DSIMULA-0105    PIC  X(013)        VALUE SPACES.
               10  COMU-NSIMULA-0105    PIC  9(009)        VALUE ZEROS.
               10  COMU-MEIOENT-0105   PIC  9(003)         VALUE ZEROS.
               10  COMU-TIPDES-0105    PIC  9(003)         VALUE ZEROS.
               10  COMU-BNCOCONV-0105  PIC  9(003)         VALUE ZEROS.
               10  COMU-AGENCONV-0105  PIC  9(005)         VALUE ZEROS.
               10  COMU-CCTACONV-0105  PIC  9(013)         VALUE ZEROS.
               10  COMU-PRODCONV-0105  PIC  9(003)         VALUE ZEROS.
               10  COMU-SPRODCONV-0105 PIC  9(003)         VALUE ZEROS.
               10  COMU-MENTCONV-0105  PIC  9(003)        VALUE ZEROS.
               10  COMU-CNPJCONV-0105.
4S2511*            15  COMU-CCNPJ-CON-0105 PIC  9(009)     VALUE ZEROS.
4S2511             15  COMU-CCNPJ-CON-0105 PIC  X(09)      VALUE SPACES.
BSI001             15  FILLER          PIC  X(001)         VALUE SPACES.
4S2511*            15  COMU-CFLIAL-CON-0105 PIC  9(004)    VALUE ZEROS.
4S2511             15  COMU-CFLIAL-CON-0105 PIC  X(04)     VALUE SPACES.

BSI001             15  FILLER          PIC  X(001)         VALUE SPACES.
                   15  COMU-CTRL-CON-0105  PIC  9(002)     VALUE ZEROS.
               10  COMU-TPDESCONV-0105 PIC  9(003)     VALUE ZEROS.

               10  COMU-MENSAGEM-0105.

                   15  COMU-COD-MSG-0105    PIC  X(004)   VALUE SPACES.
                   15  COMU-DSC-MSG-0105    PIC  X(075)   VALUE SPACES.

               10  COMU-CCART-0105          PIC  9(003)    VALUE ZEROS.
               10  COMU-NRO-CONS-NEGOC-0105.
                   15  COMU-NRCOUM-0105     PIC 9(004)     VALUE ZEROS.
                   15  COMU-NRCODOIS-0105   PIC 9(005)     VALUE ZEROS.
                   15  COMU-NRCOTRES-0105   PIC 9(009)     VALUE ZEROS.
               10  COMU-VLR-PRINC-LMTE-0105 PIC 9(015)V99  VALUE ZEROS.
               10  COMU-SLD-DISPO-LMTE-0105 PIC 9(015)V99  VALUE ZEROS.
               10  COMU-DVCTO-LMTE-0105     PIC X(010)     VALUE SPACES.
               10  COMU-CSIT-CONS-NEGOC-0105
                                            PIC 9(003)     VALUE ZEROS.
               10  COMU-SITUACAO-NEGOC-0105 PIC X(010)     VALUE SPACES.
               10  COMU-CGARNT-0105         PIC 9(003)     VALUE ZEROS.
               10  COMU-HIFEN-0105          PIC X(001)     VALUE SPACES.
               10  COMU-DSCGAR-0105         PIC X(030)     VALUE SPACES.
               10  COMU-BCOEMI-0105         PIC  9(004)    VALUE ZEROS.
               10  COMU-AGEEMI-0105         PIC  9(005)    VALUE ZEROS.
               10  COMU-CTAEMI-0105         PIC  9(013)    VALUE ZEROS.
               10  COMU-CNPJ-CPF-EMI-0105.
                   15  COMU-CCNPJ-EMI-0105  PIC  9(009)    VALUE ZEROS.
                   15  COMU-CFLIAL-EMI-0105 PIC  9(004)    VALUE ZEROS.
                   15  COMU-CTRL-EMI-0105   PIC  9(002)    VALUE ZEROS.

BRQ001*---------------------------------------------------------------*
BRQ001 01  FILLER                      PIC  X(050)         VALUE
BRQ001     '*** AREA DE COMUNICACAO DCOM3929 **'.
BRQ001*---------------------------------------------------------------*
BRQ001
BRQ001 01  WRK-COMU-AREA-3929.
BRQ001     05 WRK-COMU-LL-3929           PIC S9(004) COMP  VALUE ZEROS.
BRQ001     05 WRK-COMU-ZZ-3929           PIC S9(004) COMP  VALUE ZEROS.
BRQ001     05 WRK-COMU-TRANCODE.
BRQ001        10 WRK-COMU-TRANS-3929     PIC  X(008)       VALUE SPACES.
BRQ001        10 FILLER                  PIC  X(007)       VALUE SPACES.
BRQ001     05 WRK-COMU-SENHAS-3929       PIC  X(037)       VALUE SPACES.
BRQ001     05 WRK-COMU-COMANDO-3929      PIC  X(068)       VALUE SPACES.
BRQ001     05 WRK-COMU-DADOS-3929.
BRQ001        10 WRK-COMU-TPOPCAO-3929 PIC  X(001)         VALUE SPACES.
BRQ001        10 WRK-COMU-TMSTAMP-3929 PIC  X(026)         VALUE SPACES.
BRQ001        10 WRK-COMU-CDBASE.

BRQ001           15 WRK-COMU-CDBASE-N  PIC  9(005)         VALUE ZEROS.
BRQ001        10 WRK-COMU-RBASE        PIC  X(060)         VALUE SPACES.
BRQ001     05 WRK-COMU-MENSAGEM-3929   PIC  X(079)         VALUE SPACES.
BRQ001
BRQ001*----------------------------------------------------------------*
BRQ001 01  FILLER                      PIC  X(050)         VALUE
BRQ001    '*** AREA DE COMUNICACAO MODULO DCOM5218 ***'.
BRQ001*----------------------------------------------------------------*
BRQ001
BRQ001 01  WRK-5218-ENTRADA.
BRQ001     05 WRK-5218E-COD-RETORNO    PIC  X(004)         VALUE SPACES.
BRQ001     05 WRK-5218E-MSG-RETORNO    PIC  X(079)         VALUE SPACES.
BRQ001     05 WRK-5218E-RESTART        PIC  9(005)         VALUE ZEROS.
BRQ001     05 WRK-5218E-FLAG           PIC  X(001)         VALUE SPACES.
BRQ001     05 WRK-5218E-CFUNC-BDSCO    PIC  9(009)         VALUE ZEROS.
BRQ001     05 WRK-5218E-CTERM          PIC  X(008)         VALUE SPACES.
BRQ001     05 WRK-5218E-TPRETORNO      PIC  X(001)         VALUE SPACES.
BRQ001     05 WRK-5218E-CDBASE-RURAL   PIC  9(005)         VALUE ZEROS.
BRQ001
BRQ001 01  WRK-5218-SAIDA.
BRQ001     05 WRK-5218S-COD-RETORNO    PIC  9(004)         VALUE ZEROS.

BRQ001     05 WRK-5218S-MSG-RETORNO    PIC  X(079)         VALUE SPACES.
BRQ001     05 WRK-5218S-RESTART        PIC  9(005)         VALUE ZEROS.
BRQ001     05 WRK-5218S-FLAG           PIC  X(001)         VALUE SPACES.
BRQ001     05 WRK-5218S-QTDE-RETORNADA PIC  9(003)         VALUE ZEROS.
BRQ001     05 WRK-5218S-LISTA          OCCURS 050 TIMES.
BRQ001       10 WRK-5218S-CDBASE-RURAL PIC  9(005)         VALUE ZEROS.
BRQ001       10 WRK-5218S-CDBASE-BACEN PIC  X(003)         VALUE SPACES.
BRQ001       10 WRK-5218S-DESCRICAO    PIC  X(100)         VALUE SPACES.
BRQ001       10 WRK-5218S-STATUS       PIC  9(001)         VALUE ZEROS.
BRQ001
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE ACESSO AO MODULO DCOM5259 ***'.
      *----------------------------------------------------------------*

BSI001 01  WRK-5259-ENTRADA.
BSI001     05 WRK-5259E-COD-RETORNO    PIC  X(004)         VALUE SPACES.
BSI001     05 WRK-5259E-MSG-RETORNO    PIC  X(079)         VALUE SPACES.
BSI001     05 WRK-5259E-RESTART        PIC  9(005)         VALUE ZEROS.
BSI001     05 WRK-5259E-FLAG           PIC  X(001)         VALUE SPACES.
BSI001     05 WRK-5259E-CFUNC-BDSCO    PIC  9(009)         VALUE ZEROS.

BSI001     05 WRK-5259E-CTERM          PIC  X(008)         VALUE SPACES.
BSI001     05 WRK-5259E-CCONVE-CLI-DESC
BSI001                                 PIC  9(009)         VALUE ZEROS.
BSI001     05 WRK-5259E-CVRSAO-CONVE-CLI
BSI001                                 PIC  9(003)         VALUE ZEROS.
BSI001     05 WRK-5259E-FLAG-FUNCAO    PIC  X(001)         VALUE SPACES.
BSI001
BSI001 01  WRK-5259-SAIDA.
BSI001     05 WRK-5259S-COD-RETORNO    PIC  X(004)         VALUE SPACES.
BSI001     05 WRK-5259S-MSG-RETORNO    PIC  X(079)         VALUE SPACES.
BSI001     05 WRK-5259S-RESTART        PIC  9(005)         VALUE ZEROS.
BSI001     05 WRK-5259S-FLAG           PIC  X(001)         VALUE SPACES.
BSI001     05 WRK-5259S-CCONVE-CLI-DESC
BSI001                                 PIC  9(009)         VALUE ZEROS.
BSI001     05 WRK-5259S-CVRSAO-CONVE-CLI
BSI001                                 PIC  9(003)         VALUE ZEROS.
BSI001     05 WRK-5259S-CPRODT         PIC  9(003)         VALUE ZEROS.
BSI001     05 WRK-5259S-IPRODT         PIC  X(060)         VALUE SPACES.
BSI001     05 WRK-5259S-CSPROD-DESC-COML
BSI001                                 PIC  9(003)         VALUE ZEROS.
BSI001     05 WRK-5259S-ISPROD-DESC-COML

BSI001                                 PIC  X(040)         VALUE SPACES.
BSI001     05 WRK-5259S-CBCO-OPER-CONVE
BSI001                                 PIC  9(003)         VALUE ZEROS.
BSI001     05 WRK-5259S-IBCO-OPER-CONVE
BSI001                                 PIC  X(010)         VALUE SPACES.
BSI001     05 WRK-5259S-CAG-OPER-CONVE PIC  9(005)         VALUE ZEROS.
BSI001     05 WRK-5259S-DINCL-CONVE-CLI
BSI001                                 PIC  X(010)         VALUE SPACES.
BSI001     05 WRK-5259S-DINIC-VGCIA-CONVE
BSI001                                 PIC  X(010)         VALUE SPACES.
BSI001     05 WRK-5259S-DVCTO-CONVE-DESC
BSI001                                 PIC  X(010)         VALUE SPACES.
BSI001     05 WRK-5259S-VLIM-CONVE-CLI PIC  9(015)V9(002)  VALUE ZEROS.
BSI001     05 WRK-5259S-FLAG-NAVEGACAO-AUX
BSI001                                 PIC  X(006)         VALUE SPACES.
BSI001     05 WRK-5259S-ISIT-DESC-COML PIC  X(030)         VALUE SPACES.
4S2511     05 WRK-5259S-CCNPJ-CPF      PIC  X(009)         VALUE SPACES.
4S2511     05 WRK-5259S-CFLIAL-CNPJ    PIC  X(04)          VALUE SPACES.
BSI001     05 WRK-5259S-CCTRL-CNPJ     PIC  9(002)         VALUE ZEROS.
BSI001     05 WRK-5259S-IPSSOA-DESC-COML
BSI001                                 PIC  X(060)         VALUE SPACES.

BSI001     05 WRK-5259S-CBCO           PIC  9(003)         VALUE ZEROS.
BSI001     05 WRK-5259S-CBCO-DESC      PIC  X(020)         VALUE SPACES.
BSI001     05 WRK-5259S-CAG-BCRIA      PIC  9(005)         VALUE ZEROS.
BSI001     05 WRK-5259S-CAG-BCRIA-DESC PIC  X(030)         VALUE SPACES.
BSI001     05 WRK-5259S-CCTA-BCRIA-CLI PIC  9(013)         VALUE ZEROS.
BSI001     05 WRK-5259S-CINDCD-CONVE-ROTVO
BSI001                                 PIC  X(001)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*** AREA DE COMUNICACAO DCOM4174 ***'.
      *----------------------------------------------------------------*

       COPY 'I#DCOMXJ'.

BSI001*----------------------------------------------------------------*
BSI001 01  FILLER                      PIC  X(050)         VALUE
BSI001     '*** AREA DE ACESSO AO MODULO DCOM5424 ***'.
BSI001*----------------------------------------------------------------*
BSI001
BSI001 01  WRK-AREA-DCOM5424.

BSI001     05 WRK-5424-ENTRADA.
BSI001        10 WRK-5424E-COD-RETORNO PIC  X(004)         VALUE SPACES.
BSI001        10 WRK-5424E-MSG-RETORNO PIC  X(079)         VALUE SPACES.
BSI001        10 WRK-5424E-RESTART     PIC  9(005)         VALUE ZEROS.
BSI001        10 WRK-5424E-FLAG        PIC  X(001)         VALUE SPACES.
BSI001        10 WRK-5424E-CFUNC-BDSCO PIC  9(009)         VALUE ZEROS.
BSI001        10 WRK-5424E-CTERM       PIC  X(008)         VALUE SPACES.
BSI001        10 WRK-5424E-OPERACAO    PIC  9(013)         VALUE ZEROS.
BSI001        10 WRK-5424E-CPRODT      PIC  9(003)         VALUE ZEROS.
BSI001        10 WRK-5424E-CSPROD-DESC-COML
BSI001                                 PIC  9(003)         VALUE ZEROS.
BSI001
BSI001     03 WRK-5424-SAIDA.
BSI001        05 WRK-5424S-COD-RETORNO PIC  9(004)         VALUE ZEROS.
BSI001        05 WRK-5424S-MSG-RETORNO PIC  X(079)         VALUE SPACES.
BSI001        05 WRK-5424S-CIDTFD-TX-REDE
BSI001                                 PIC  X(001)         VALUE SPACES.
BSI001        05 WRK-5424S-CIDTFD-TX-MIN-VLR
BSI001                                 PIC  9(013)V9(002)  VALUE ZEROS.
BSI001        05 WRK-5424S-CIDTFD-TX-MAX-VLR
BSI001                                 PIC  9(013)V9(002)  VALUE ZEROS.


      *----------------------------------------------------------------*
BSI001 01  FILLER                      PIC  X(050)         VALUE
BSI001     '*** AREA DE ACESSO AO MODULO DCOM5475 ***'.
BSI001*----------------------------------------------------------------*
BSI001
BSI001 01  WRK-5475-ENTRADA.
BSI001     05 WRK-5475E-COD-RETORNO    PIC  X(004)         VALUE SPACES.
BSI001     05 WRK-5475E-MSG-RETORNO    PIC  X(079)         VALUE SPACES.
BSI001     05 WRK-5475E-RESTART        PIC  9(005)         VALUE ZEROS.
BSI001     05 WRK-5475E-FLAG           PIC  X(001)         VALUE SPACES.
BSI001     05 WRK-5475E-CFUNC-BDSCO    PIC  9(009)         VALUE ZEROS.
BSI001     05 WRK-5475E-CTERM          PIC  X(008)         VALUE SPACES.
BSI001     05 WRK-5475E-FLAG-FILTRO    PIC  X(002)         VALUE SPACES.
BSI001     05 WRK-5475E-OPERACAO       PIC  9(013)         VALUE ZEROS.
BSI001     05 FILLER                   REDEFINES WRK-5475E-OPERACAO.
BSI001        10 WRK-5475E-DANO-OPER-DESC
BSI001                                 PIC  9(004).
BSI001        10 WRK-5475E-NSEQ-OPER-DESC
BSI001                                 PIC  9(009).
BSI001     05 WRK-5475E-NPCELA-DESC-COML

BSI001                                 PIC  9(005)         VALUE ZEROS.

BSI001 01  WRK-5475-SAIDA.
BSI001     05 WRK-5475S-COD-RETORNO    PIC  X(004)         VALUE SPACES.
BSI001     05 WRK-5475S-MSG-RETORNO    PIC  X(079)         VALUE SPACES.
BSI001     05 WRK-5475S-RESTART        PIC  9(005)         VALUE ZEROS.
BSI001     05 WRK-5475S-FLAG           PIC  X(001)         VALUE SPACES.
BSI001     05 WRK-5475S-MOTVO-ADITAMENTO
BSI001                                 PIC  X(001)         VALUE SPACES.
BSI001     05 WRK-5475S-FLAG-ADT-TITLO PIC  X(001)         VALUE SPACES.
BSI001     05 WRK-5475S-NPCELA-DESC    PIC  9(005)         VALUE ZEROS.
BSI001     05 WRK-5475S-DVCTO-PCELA-DESC-DE
BSI001                                 PIC  X(010)         VALUE SPACES.
BSI001     05 WRK-5475S-PTX-JURO-MES-DE
BSI001                                 PIC  9(003)V9(005)  VALUE ZEROS.
BSI001     05 WRK-5475S-VALOR-PCELA-DE PIC  9(015)V9(002)  VALUE ZEROS.
BSI001     05 WRK-5475S-CTPO-TAXA-DE   PIC  9(003)         VALUE ZEROS.
BSI001     05 WRK-5475S-ITPO-TAXA-DE   PIC  X(015)         VALUE SPACES.
BSI001     05 WRK-5475S-CSIT-PARCELA-DE
BSI001                                 PIC  9(003)         VALUE ZEROS.
BSI001     05 WRK-5475S-ISIT-PARCELA-DE

BSI001                                 PIC  X(015)         VALUE SPACES.
BSI001     05 WRK-5475S-DVCTO-PCELA-DESC-PA
BSI001                                 PIC  X(010)         VALUE SPACES.
BSI001     05 WRK-5475S-PTX-JURO-MES-PA
BSI001                                 PIC  9(003)V9(005)  VALUE ZEROS.
BSI001     05 WRK-5475S-VJURO-PRROG-PCELA-PA
BSI001                                 PIC  9(015)V9(002)  VALUE ZEROS.
BSI001     05 WRK-5475S-PIOF-PRRG-PCELA-PA
BSI001                                 PIC  9(003)V9(005)  VALUE ZEROS.
BSI001     05 WRK-5475S-VIOF-PRROG-PCELA-PA
BSI001                                 PIC  9(015)V9(002)  VALUE ZEROS.
BSI001     05 WRK-5475S-VTAXA-PRRG-PCELA-PA
BSI001                                 PIC  9(015)V9(002)  VALUE ZEROS.
BSI001     05 WRK-5475S-CAUTRZ-ESPCL   PIC  9(009)         VALUE ZEROS.
BSI001     05 WRK-5475S-IAUTRZ-ESPCL   PIC  X(040)         VALUE SPACES.
BRQ001     05 WRK-5475S-REGISTRAR-BACEN
BRQ001                                 PIC  X(001)         VALUE SPACES.

BSI001*----------------------------------------------------------------*
BSI001 01  FILLER                      PIC  X(050)         VALUE
BSI001     '*** AREA DE ACESSO AO MODULO DCOM5151 ***'.

BSI001*----------------------------------------------------------------*
BSI001
BSI001 01  WRK-5151-ENTRADA.
BSI001     05 WRK-5151E-COD-RETORNO    PIC  X(004)         VALUE SPACES.
BSI001     05 WRK-5151E-MSG-RETORNO    PIC  X(079)         VALUE SPACES.
BSI001     05 WRK-5151E-RESTART        PIC  9(005)         VALUE ZEROS.
BSI001     05 WRK-5151E-FLAG           PIC  X(001)         VALUE SPACES.
BSI001     05 WRK-5151E-CFUNC-BDSCO    PIC  9(009)         VALUE ZEROS.
BSI001     05 WRK-5151E-CTERM          PIC  X(008)         VALUE SPACES.
BSI001     05 WRK-5151E-OPERACAO       PIC  9(013)         VALUE ZEROS.
4S2511     05 WRK-5151E-CCNPJ-CPF      PIC  X(009)         VALUE SPACES.
4S2511     05 WRK-5151E-CFLIAL-CNPJ    PIC  X(04)          VALUE SPACES.
BSI001     05 WRK-5151E-DVCTO-ANTE     PIC  X(010)         VALUE SPACES.
BSI001     05 WRK-5151E-DVCTO-NOVA     PIC  X(010)         VALUE SPACES.
BSI001     05 WRK-5151E-VPRINC-PCELA   PIC  9(015)V9(002)  VALUE ZEROS.
BSI001
BSI001 01  WRK-5151-SAIDA.
BSI001     05 WRK-5151S-COD-RETORNO    PIC  X(004)         VALUE SPACES.
BSI001     05 WRK-5151S-MSG-RETORNO    PIC  X(079)         VALUE SPACES.
BSI001     05 WRK-5151S-RESTART        PIC  9(005)         VALUE ZEROS.
BSI001     05 WRK-5151S-FLAG           PIC  X(001)         VALUE SPACES.

BSI001     05 WRK-5151S-CINDCD-TX-REDE PIC  X(001)         VALUE SPACES.
BSI001     05 WRK-5151S-CTPO-TX-JURO-DESC
BSI001                                 PIC  9(003)         VALUE ZEROS.
BSI001     05 WRK-5151S-CSGL-TPO-TX-JURO
BSI001                                 PIC  X(006)         VALUE SPACES.
BSI001     05 WRK-5151S-PMIN-TX-JURO-REGRA
BSI001                                 PIC  9(003)V9(006)  VALUE ZEROS.
BSI001     05 WRK-5151S-PMAX-TX-JURO-REGRA
BSI001                                 PIC  9(003)V9(006)  VALUE ZEROS.
BSI001     05 WRK-5151S-CINDCD-FREQ-TX PIC  X(002)         VALUE SPACES.

BSI001*----------------------------------------------------------------*
BSI001 01  FILLER                      PIC  X(050)         VALUE
BSI001     '*** AREA DE ACESSO AO MODULO DCOM5470 ***'.
BSI001*----------------------------------------------------------------*
BSI001
BSI001 01  WRK-5470-ENTRADA.
BSI001     05 WRK-5470E-COD-RETORNO    PIC  X(004)         VALUE SPACES.
BSI001     05 WRK-5470E-MSG-RETORNO.
BSI001        10 WRK-5470E-COD-DEPTO   PIC  9(004)         VALUE ZEROS.
BSI001        10 WRK-FILLER            PIC  X(075)         VALUE SPACES.

BSI001     05 WRK-5470E-RESTART        PIC  9(005)         VALUE ZEROS.
BSI001     05 WRK-5470E-FLAG           PIC  X(001)         VALUE SPACES.
BSI001     05 WRK-5470E-CFUNC-BDSCO    PIC  9(009)         VALUE ZEROS.
BSI001     05 WRK-5470E-CTERM          PIC  X(008)         VALUE SPACES.
BSI001     05 WRK-5470E-PRODUTO        PIC  9(005)         VALUE ZEROS.
BSI001     05 WRK-5470E-SUBPRODUTO     PIC  9(005)         VALUE ZEROS.
BSI001     05 WRK-5470E-BANCO          PIC  9(003)         VALUE ZEROS.
BSI001     05 WRK-5470E-AGENCIA        PIC  9(005)         VALUE ZEROS.
BSI001     05 WRK-5470E-CONTA          PIC  9(007)         VALUE ZEROS.
BSI001     05 WRK-5470E-CNPJ-CPF-CEDENTE.
4S2511        10 WRK-5470E-CCNPJ-CPF-CEDENTE
4S2511                                 PIC  X(009)         VALUE SPACES.
4S2511        10 WRK-5470E-CFLIAL-CNPJ-CEDENTE
4S2511                                 PIC  X(04)          VALUE SPACES.
BSI001        10 WRK-5470E-CCTRL-CNPJ-CEDENTE
BSI001                                 PIC  9(002)         VALUE ZEROS.
BSI001     05 WRK-5470E-OPERACAO       PIC  9(013)         VALUE ZEROS.
BSI001     05 FILLER                   REDEFINES WRK-5470E-OPERACAO.
BSI001        10 WRK-5470E-DANO-OPER-DESC
BSI001                                 PIC  9(004).
BSI001        10 WRK-5470E-NSEQ-OPER-DESC

BSI001                                 PIC  9(009).
BSI001
BSI001**************** DADOS PARCELA ***********************************
BSI001     05 WRK-5470E-TP-DESCONTO    PIC  9(003)         VALUE ZEROS.
BSI001     05 WRK-5470E-NPCELA-DESC-COML
BSI001                                 PIC  9(005)         VALUE ZEROS.
BSI001     05 WRK-5470E-MOTVO-ADIT     PIC  X(001)         VALUE SPACES.
BSI001     05 WRK-5470E-FLAG-CONSIST   PIC  X(001)         VALUE SPACES.
BSI001     05 WRK-5470E-CCAR-CONS-CONE PIC  X(005)         VALUE SPACES.
BSI001     05 WRK-5470E-DANO-CONS-CONE PIC  9(004)         VALUE ZEROS.
BSI001     05 WRK-5470E-CAGE-CONS-CONE PIC  9(005)         VALUE ZEROS.
BSI001     05 WRK-5470E-NSEQ-CONS-CONE PIC  9(009)         VALUE ZEROS.
BSI001     05 WRK-5470E-PTX-JURO-MES-DE
BSI001                                 PIC S9(003)V9(005)  VALUE ZEROS.
BSI001     05 WRK-5470E-TFREQ-TAXA-PCELA-DE
BSI001                                 PIC  X(004)         VALUE SPACES.
BSI001     05 WRK-5470E-VLR-PARCELA    PIC S9(015)V9(002)  VALUE ZEROS.
BSI001     05 WRK-5470E-DVCTO-PCELA-PARA
BSI001                                 PIC  X(010)         VALUE SPACES.
BSI001     05 WRK-5470E-PTX-JURO-MES-PARA
BSI001                                 PIC S9(003)V9(005)  VALUE ZEROS.

BSI001     05 WRK-5470E-VJURO-PRROG-PARA
BSI001                                 PIC S9(015)V9(002)  VALUE ZEROS.
BSI001     05 WRK-5470E-PIOF-PRROG-PARA
BSI001                                 PIC S9(003)V9(005)  VALUE ZEROS.
BSI001     05 WRK-5470E-VIOF-PRROG-PARA
BSI001                                 PIC S9(015)V9(002)  VALUE ZEROS.
BSI001     05 WRK-5470E-VTAXA-PRROG-PARA
BSI001                                 PIC S9(015)V9(002)  VALUE ZEROS.
BSI001     05 WRK-5470E-TFREQ-TAXA-PARA
BSI001                                 PIC  X(004)         VALUE SPACES.
BSI001     05 WRK-5470E-PMIN-TAXA-JURO-PARA
BSI001                                 PIC S9(003)V9(003)  VALUE ZEROS.
BSI001     05 WRK-5470E-PMAX-TAXA-JURO-PARA
BSI001                                 PIC S9(003)V9(003)  VALUE ZEROS.
BSI001     05 WRK-5470E-CTPO-TX-JURO   PIC  9(003)         VALUE ZEROS.
BSI001     05 WRK-5470E-ITPO-TX-JURO   PIC  X(015)         VALUE SPACES.
BSI001     05 WRK-5470E-COD-AUTORIZANTE
BSI001                                 PIC  9(009)         VALUE ZEROS.
BSI001     05 WRK-5470E-DESC-AUTORIZANTE
BSI001                                 PIC  X(040)         VALUE SPACES.
BRQ001     05 WRK-5470E-CDBASE-RURAL   PIC  9(005)         VALUE ZEROS.

BSI001
BSI001 01  WRK-5470-SAIDA.
BSI001     05 WRK-5470S-COD-RETORNO    PIC  X(004)         VALUE SPACES.
BSI001     05 WRK-5470S-MSG-RETORNO    PIC  X(079)         VALUE SPACES.
BSI001     05 WRK-5470S-TAMANHO-ERRO   PIC  9(001)         VALUE ZEROS.
BSI001     05 WRK-5470S-TABELA-ERROS   OCCURS 016 TIMES.
BSI001        10 WRK-5470S-CAMPOS-ERRO PIC  9(002)         VALUE ZEROS.
BSI001
BSI001*----------------------------------------------------------------*
BSI001 01  FILLER                      PIC  X(050)         VALUE
BSI001     '*** AREA DE ACESSO AO MODULO DCOM5471 ***'.
BSI001*----------------------------------------------------------------*
BSI001
BSI001 01  WRK-5471-ENTRADA.
BSI001     05 WRK-5471E-COD-RETORNO    PIC  X(004)         VALUE SPACES.
BSI001     05 WRK-5471E-MSG-RETORNO    PIC  X(079)         VALUE SPACES.
BSI001     05 WRK-5471E-CFUNC-BDSCO    PIC  9(009)         VALUE ZEROS.
BSI001     05 WRK-5471E-CTERM          PIC  X(008)         VALUE SPACES.
BSI001     05 WRK-5471E-PRODUTO        PIC  9(005)         VALUE ZEROS.
BSI001     05 WRK-5471E-SUBPRODUTO     PIC  9(005)         VALUE ZEROS.
BSI001     05 WRK-5471E-OPERACAO       PIC  9(013)         VALUE ZEROS.

BSI001     05 FILLER                   REDEFINES WRK-5471E-OPERACAO.
BSI001        10 WRK-5471E-DANO-OPER-DESC
BSI001                                 PIC  9(004).
BSI001        10 WRK-5471E-NSEQ-OPER-DESC
BSI001                                 PIC  9(009).
BSI001     05 WRK-5471E-NPCELA-DESC-COML
BSI001                                 PIC  9(005)         VALUE ZEROS.
BSI001     05 WRK-5471E-DVCTO-PCELA-DESC
BSI001                                 PIC  X(010)         VALUE SPACES.
BSI001     05 WRK-5471E-VPRINC-PCELA-DESC
BSI001                                 PIC S9(015)V9(002)  VALUE ZEROS.
BSI001     05 WRK-5471E-PTX-JURO-MES   PIC  9(003)V9(005)  VALUE ZEROS.
BSI001     05 WRK-5471E-DVCTO-PCELA-PRROG
BSI001                                 PIC  X(010)         VALUE SPACES.
BSI001     05 WRK-5471E-PTX-JURO-MES-PRROG
BSI001                                 PIC  9(003)V9(005)  VALUE ZEROS.

BSI001 01  WRK-5471-SAIDA.
BSI001     05 WRK-5471S-COD-RETORNO    PIC  X(004)         VALUE SPACES.
BSI001     05 WRK-5471S-MSG-RETORNO    PIC  X(079)         VALUE SPACES.
BSI001     05 WRK-5471S-TAMANHO-ERRO   PIC  9(001)         VALUE ZEROS.

BSI001     05 WRK-5471S-TABELA-ERROS   OCCURS 010  TIMES.
BSI001        10 WRK-5471S-CAMPOS-ERRO PIC  9(002)         VALUE ZEROS.
BSI001     05 WRK-5471S-VALOR-JUROS    PIC S9(015)V9(002)  VALUE ZEROS.
BSI001     05 WRK-5471S-VALOR-IOF      PIC S9(015)V9(002)  VALUE ZEROS.
BSI001     05 WRK-5471S-TARIFA-PRROG   PIC S9(015)V9(002)  VALUE ZEROS.
BSI001     05 WRK-5471S-VALOR-ENC      PIC S9(015)V9(002)  VALUE ZEROS.
BSI001
PRIME *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*** AREA DE COMUNICACAO DCOM5549 ***'.
      *----------------------------------------------------------------*

       01  WRK-5549-ENTRADA.
          05 WRK-5549-E-COD-RETORNO    PIC  X(004)         VALUE SPACES.
          05 WRK-5549-E-MSG-RETORNO    PIC  X(079)         VALUE SPACES.
          05 WRK-5549-E-RESTART        PIC  9(005)         VALUE ZEROS.
          05 WRK-5549-E-FLAG           PIC  X(001)         VALUE SPACES.
          05 WRK-5549-E-CFUNC          PIC  9(009)         VALUE ZEROS.
          05 WRK-5549-E-CTERM          PIC  X(008)         VALUE SPACES.
          05 WRK-5549-E-DADOS.
             10 WRK-5549-E-CFUNC-BDSCO PIC  9(009)         VALUE ZEROS.

             10 WRK-5549-E-DEPTO       PIC  9(005)         VALUE ZEROS.
          05 FILLER                    PIC  X(100)         VALUE SPACES.

       01  WRK-5549-SAIDA.
          05 WRK-5549-S-HEADER.
            10 WRK-5549-S-COD-RETORNO  PIC  X(004)         VALUE SPACES.
            10 WRK-5549-S-MSG-RETORNO  PIC  X(079)         VALUE SPACES.
            10 WRK-5549-S-RESTART      PIC  9(005)         VALUE ZEROS.
            10 WRK-5549-S-FLAG         PIC  X(001)         VALUE SPACES.
          05 WRK-5549-S-DADOS.
             10 WRK-5549-S-CFUNC-BDSCO PIC  9(009)         VALUE ZEROS.
             10 WRK-5549-S-DEPTO       PIC  9(005)         VALUE ZEROS.
      * WRK-5549-S-AG-DEPTO = 'A' > AGENCIA
      * WRK-5549-S-AG-DEPTO = 'D' > DEPARTAMENTO
             10 WRK-5549-S-AG-DEPTO    PIC  X(001)         VALUE SPACES.
          05 FILLER                    PIC  X(100)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)      VALUE
           '*** AREAS AUXILIARES PARA BRAD5010 ***'.
      *----------------------------------------------------------------*


       01  WRK-BRAD5010.
           05  WRK-SQLCA               PIC  X(136)         VALUE SPACES.
           05  WRK-GU                  PIC  X(04)          VALUE 'GU  '.
           05  WRK-CHNG                PIC  X(04)          VALUE 'CHNG'.
           05  WRK-ISRT                PIC  X(04)          VALUE 'ISRT'.
           05  WRK-FUNCAO              PIC  X(04)          VALUE SPACES.
           05  WRK-TELA                PIC  X(08)          VALUE SPACES.
           05  WRK-VERSAO              PIC  X(06)          VALUE
                                                           'VRS002'.
           05  WRK-COD-USER            PIC  X(07)          VALUE SPACES.
           05  WRK-COD-USER-R          REDEFINES
               WRK-COD-USER            PIC  9(07).
           05  WRK-COD-DEPTO.
               10  FILLER             PIC   X(02)          VALUE SPACES.
               10  WRK-COD-DEPTO-N    PIC   9(04)          VALUE ZEROS.

           05  WRK-MENSAGEM.
               10  FILLER              PIC  X(04)          VALUE SPACES.
               10  WRK-TRANSACAO       PIC  X(08)          VALUE SPACES.
               10  FILLER              PIC  X(1988)        VALUE SPACES.

      *
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*** AREA DA TELA DCOMRCTM - INPUT ***'.
      *----------------------------------------------------------------*

       01  WRK-INPUT-RCTM.
           05  FILLER                     PIC  X(18)       VALUE SPACES.
           05  WRK-INP-PFK-RCTM           PIC  X(01)       VALUE SPACES.
           05  WRK-INP-SENHAS-RCTM        PIC  X(37)       VALUE SPACES.
           05  WRK-INP-COMANDO-RCTM       PIC  X(68)       VALUE SPACES.
           05  WRK-INP-DADOS-RCTM.
               10 WRK-INP-POSI-RCTM.
                  15 WRK-INP-LINHA-RCTM   PIC S9(004) COMP VALUE ZEROS.
                  15 WRK-INP-COLUN-RCTM   PIC S9(004) COMP VALUE ZEROS.
               10  WRK-INP-TIPOOPC-RCTM
                                          PIC  X(01)       VALUE SPACES.
               10  WRK-INP-TMSTAMP-RCTM
                                          PIC  X(26)       VALUE SPACES.
               10  WRK-INP-PASSO-RCTM     PIC  X(01)       VALUE SPACES.
               10  WRK-INP-CHAMADO-RCTM

                                          PIC  X(08)       VALUE SPACES.
               10  WRK-INP-PAGINA-RCTM    PIC  X(04)       VALUE SPACES.
               10  WRK-INP-TPSEL-RCTM     PIC  X(01)       VALUE SPACES.
               10  WRK-INP-CODDESC-RCTM   PIC 9(03)        VALUE ZEROS.
               10  WRK-INP-VLRIOF-RCTM    PIC 9(03)V9(05)  VALUE ZEROS.
               10  WRK-INP-PAGINA1-RCTM   PIC  X(04)       VALUE SPACES.
               10  WRK-INP-TECLPF9-RCTM   PIC  X(01)       VALUE SPACES.
               10  WRK-INP-NOMECL-RCTM    PIC  X(40)       VALUE SPACES.
BSI001         10  WRK-INP-CGCCPF-RCTM.
4S2511*            15 WRK-INP-CNPJ1-RCTM  PIC  9(03)       VALUE ZEROS.
4S2511             15 WRK-INP-CNPJ1-RCTM  PIC  X(03)       VALUE SPACES.
BSI001             15 FILLER              PIC  X(01)       VALUE SPACES.
4S2511*            15 WRK-INP-CNPJ2-RCTM  PIC  9(03)       VALUE ZEROS.
4S2511            15 WRK-INP-CNPJ2-RCTM  PIC   X(03)       VALUE SPACES.
BSI001             15 FILLER              PIC  X(01)       VALUE SPACES.
4S2511*            15 WRK-INP-CNPJ3-RCTM  PIC  9(03)       VALUE ZEROS.
4S2511             15 WRK-INP-CNPJ3-RCTM  PIC  X(03)       VALUE SPACES.
BSI001             15 FILLER              PIC  X(01)       VALUE SPACES.
4S2511*            15 WRK-INP-CNPJ4-RCTM  PIC  9(04)       VALUE ZEROS.
4S2511             15 WRK-INP-CNPJ4-RCTM  PIC  X(04)       VALUE SPACES.
BSI001             15 FILLER              PIC  X(01)       VALUE SPACES.
BSI001             15 WRK-INP-CNPJ5-RCTM  PIC  9(02)       VALUE ZEROS.
               10  WRK-INP-XCONTA-RCTM    PIC  X(23)       VALUE SPACES.
               10  WRK-INP-CODPRD-RCTM    PIC  9(03)       VALUE ZEROS.
               10  WRK-INP-DESPRD-RCTM    PIC  X(12)       VALUE SPACES.

               10  WRK-INP-CODSUB-RCTM    PIC  9(03)       VALUE ZEROS.
               10  WRK-INP-DESSUB-RCTM    PIC  X(12)       VALUE SPACES.
               10  WRK-INP-OPERAC-RCTM    PIC  9(13)       VALUE ZEROS.
BRQ001         10  FILLER REDEFINES       WRK-INP-OPERAC-RCTM.
BRQ001             15 WRK-INP-DANO-RCTM   PIC  9(04).
BRQ001             15 WRK-INP-NSEQ-RCTM   PIC  9(09).
               10  WRK-INP-SITUAC-RCTM    PIC  X(15)       VALUE SPACES.
               10  WRK-INP-DESIDE-RCTM    PIC  X(13)       VALUE SPACES.
BSI001         10  WRK-INP-NUMERO-RCTM.
BSI001             15 WRK-INP-NUMERO-RCTM-N
BSI001                                    PIC  9(09)       VALUE ZEROS.
               10  WRK-INP-PARCEL-RCTM    PIC  9999        VALUE ZEROS.
               10  WRK-INP-DTVENC-RCTM    PIC  X(10)       VALUE SPACES.
               10  WRK-INP-VALOR-RCTM     PIC  ZZ.ZZZ.ZZZ.ZZ9,99
                                                           VALUE ZEROS.
               10  WRK-INP-TAXA-RCTM      PIC  ZZ9,99      VALUE ZEROS.
               10  WRK-INP-FREQTXOP-RCTM  PIC  X(03)       VALUE SPACES.
               10  WRK-INP-TPJUR-RCTM     PIC  9(03)       VALUE ZEROS.
               10  WRK-INP-TSMIN-RCTM.
                   15 WRK-INP-TSMIN-RCTM-N
                                          PIC  9(16)V99    VALUE ZEROS.

               10  WRK-INP-TSMAX-RCTM.
                   15 WRK-INP-TSMAX-RCTM-N
                                          PIC  9(16)V99    VALUE ZEROS.
               10  WRK-INP-FLAG-RCTM      PIC  X(01)       VALUE SPACES.
               10  WRK-INP-CHAMA-RCTM     PIC  X(08)       VALUE SPACES.
               10  WRK-INP-OPER-RCTM      PIC  9(13)       VALUE ZEROS.
               10  WRK-INP-DATA-PRORROGADA.
                  15  WRK-INP-PRODIA-RCTM.
                     20 WRK-INP-PRODIA-RCTM-N PIC 9(02)   VALUE ZEROS.
                  15  WRK-INP-PROMES-RCTM.
                     20 WRK-INP-PROMES-RCTM-N PIC 9(02)   VALUE ZEROS.
                  15  WRK-INP-PROANO-RCTM.
                     20 WRK-INP-PROANO-RCTM-N PIC 9(04)   VALUE ZEROS.
               10  WRK-INP-CARTE-RCTM     PIC  X(03)       VALUE SPACES.
               10  WRK-INP-AGCONE-RCTM    PIC  9(05)       VALUE ZEROS.
               10  WRK-INP-TXJURO-RCTM.
                  15  WRK-INP-TXJURO-RCTM-N PIC  ZZ9,99999 VALUE ZEROS.
               10  WRK-INP-FREQTX-RCTM PIC  X(04)          VALUE SPACES.
BRQ001         10  WRK-INP-TPJURO-RCTM PIC  X(13)          VALUE SPACES.
               10 WRK-INP-MINMAX-RCTM.
                 15 FILLER              PIC  X(06)         VALUE SPACES.

                 15 WRK-INP-TXMIN-RCTM  PIC  99,999  VALUE ZEROS.
                 15 FILLER              PIC  X(06)    VALUE SPACES.
                 15 WRK-INP-TXMAX-RCTM  PIC  99,999   VALUE ZEROS.
                 15 FILLER              PIC  X(01)         VALUE SPACES.
BRQ001         10 WRK-INP-LTBSPR-RCTM   PIC  X(17)         VALUE SPACES.
BRQ001         10 WRK-INP-CDBSPR-RCTM.
BRQ001           15 WRK-INP-CDBSPR-RCTM-N
BRQ001                                  PIC  9(05)         VALUE ZEROS.
BRQ001         10 WRK-INP-DSBSPR-RCTM   PIC  X(55)         VALUE SPACES.
               10 WRK-INP-LITAUT-RCTM   PIC  X(12)         VALUE SPACES.
               10 WRK-INP-CODAUT-RCTM.
                 15 WRK-INP-CODAUT-RCTM-N
                                        PIC  9(09)         VALUE ZEROS.
               10 WRK-INP-DESAUT-RCTM   PIC  X(22)        VALUE SPACES.
               10 WRK-INP-JAUTOR-RCTM.
                15 WRK-INP-JAUTOR-RCTM-N PIC ZZZ.ZZZ.ZZ9,99 VALUE ZEROS.
               10 WRK-INP-IAUTOR-RCTM.
                15 WRK-INP-IAUTOR-RCTM-N PIC ZZZ.ZZZ.ZZ9,99 VALUE ZEROS.
               10 WRK-INP-TACAUT-RCTM.
                15 WRK-INP-TACAUT-RCTM-N PIC ZZZ.ZZZ.ZZ9,99 VALUE ZEROS.
               10 WRK-INP-TOTAUT-RCTM.

                15 WRK-INP-TOTAUT-RCTM-N PIC ZZZ.ZZZ.ZZ9,99 VALUE ZEROS.
           05  WRK-INP-MENSA-RCTM       PIC  X(79)          VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                          PIC  X(50)      VALUE
           '*** AREA DA TELA DCOMRCTM  - OUTPUT ***'.
      *----------------------------------------------------------------*
       01  WRK-OUTPUT-RCTM.
           05  WRK-OUT-LL-RCTM         PIC S9(04) COMP     VALUE ZEROS.
           05  WRK-OUT-ZZ-RCTM         PIC S9(04) COMP     VALUE ZEROS.
           05  WRK-OUT-SENHAS-RCTM     PIC  X(37)          VALUE SPACES.
           05  WRK-OUT-COMANDO-ATTR-RCTM PIC S9(04) COMP   VALUE ZEROS.
           05  WRK-OUT-COMANDO-RCTM    PIC  X(68)          VALUE SPACES.
           05  WRK-OUT-DADOS-RCTM.
               10  WRK-OUT-TIPOOPC-RCTM
                                       PIC  X(01)          VALUE SPACES.
               10  WRK-OUT-TMSTAMP-RCTM
                                       PIC  X(26)          VALUE SPACES.
               10  WRK-OUT-PASSO-RCTM  PIC  X(01)          VALUE SPACES.
               10  WRK-OUT-CHAMADO-RCTM
                                       PIC  X(08)          VALUE SPACES.

               10  WRK-OUT-PAGINA-RCTM PIC  X(04)          VALUE SPACES.
               10  WRK-OUT-TPSEL-RCTM  PIC  X(01)          VALUE SPACES.
               10  WRK-OUT-CODDESC-RCTM PIC  9(03)         VALUE ZEROS.
               10  WRK-OUT-VLRIOF-RCTM PIC 9(03)V9(05)    VALUE ZEROS.
               10  WRK-OUT-PAGINA1-RCTM PIC  X(04)         VALUE SPACES.
               10  WRK-OUT-TECLPF9-RCTM PIC  X(01)         VALUE SPACES.
               10  WRK-OUT-NOMECL-RCTM PIC  X(40)          VALUE SPACES.
               10  WRK-OUT-CGCCPF-RCTM PIC  X(19)          VALUE SPACES.
               10  WRK-OUT-XCONTA-RCTM PIC  X(23)          VALUE SPACES.
               10  WRK-OUT-CODPRD-RCTM PIC  9(03)          VALUE ZEROS.
               10  WRK-OUT-DESPRD-RCTM PIC  X(12)          VALUE SPACES.
               10  WRK-OUT-CODSUB-RCTM PIC  9(03)          VALUE ZEROS.
               10  WRK-OUT-DESSUB-RCTM PIC  X(12)          VALUE SPACES.
               10  WRK-OUT-OPERAC-RCTM PIC  9(13)          VALUE ZEROS.
               10  WRK-OUT-SITUAC-RCTM PIC  X(15)          VALUE SPACES.
               10  WRK-OUT-DESIDE-RCTM PIC  X(13)          VALUE SPACES.
               10  WRK-OUT-NUMERO-RCTM PIC  X(09)          VALUE SPACES.
               10  WRK-OUT-PARCEL-RCTM PIC  9999           VALUE ZEROS.
               10  WRK-OUT-DTVENC-RCTM PIC  X(10)          VALUE SPACES.
               10  WRK-OUT-VALOR-RCTM  PIC  ZZ.ZZZ.ZZZ.ZZ9,99
                                                           VALUE ZEROS.

               10  WRK-OUT-TAXA-RCTM   PIC  ZZ9,99         VALUE ZEROS.
               10  WRK-OUT-FREQTXOP-RCTM
                                       PIC  X(03)          VALUE SPACES.
               10  WRK-OUT-TPJUR-RCTM  PIC  9(03)          VALUE ZEROS.
               10  WRK-OUT-TSMIN-RCTM.
                   15 WRK-OUT-TSMIN-RCTM-N
                                       PIC  9(16)V99       VALUE ZEROS.
               10  WRK-OUT-TSMAX-RCTM.
                   15 WRK-OUT-TSMAX-RCTM-N
                                       PIC  9(16)V99       VALUE ZEROS.
               10  WRK-OUT-FLAG-RCTM   PIC  X(01)          VALUE SPACES.
               10  WRK-OUT-CHAMA-RCTM  PIC  X(08)         VALUE SPACES.
               10  WRK-OUT-OPER-RCTM   PIC  9(13)         VALUE ZEROS.
               10  WRK-OUT-DATA-PRORROGADA.
                   15  WRK-OUT-PRODIA-ATTR-RCTM
                                       PIC S9(04) COMP     VALUE ZEROS.
                   15  WRK-OUT-PRODIA-RCTM PIC  X(02)    VALUE SPACES.
                   15  WRK-OUT-PROMES-ATTR-RCTM
                                       PIC S9(04) COMP     VALUE ZEROS.
                   15  WRK-OUT-PROMES-RCTM PIC  X(02)    VALUE SPACES.
                   15  WRK-OUT-PROANO-ATTR-RCTM

                                       PIC S9(04) COMP     VALUE ZEROS.
                   15  WRK-OUT-PROANO-RCTM PIC  X(04)    VALUE SPACES.
               10  WRK-OUT-CARTE-RCTM     PIC  X(03)     VALUE SPACES.
               10  WRK-OUT-AGCONE-RCTM-X.
                   15  WRK-OUT-AGCONE-RCTM PIC  9(05)     VALUE ZEROS.
               10  WRK-OUT-TXJURO-ATTR-RCTM
                                       PIC S9(04) COMP     VALUE ZEROS.
               10  WRK-OUT-TXJURO-RCTM.
                   15  WRK-OUT-TXJURO-RCTM-N PIC  ZZ9,99999 VALUE ZEROS.
               10  WRK-OUT-FREQTX-RCTM PIC  X(04)          VALUE SPACES.
               10  WRK-OUT-TPJURO-ATTR-RCTM
                                       PIC S9(04) COMP     VALUE ZEROS.
BRQ001         10  WRK-OUT-TPJURO-RCTM PIC  X(13)          VALUE SPACE.
               10  WRK-OUT-MINMAX-ATTR-RCTM
                                       PIC S9(004) COMP    VALUE ZEROS.
               10 WRK-OUT-MINMAX-RCTM.
                 15 FILLER              PIC  X(06)         VALUE SPACES.
                 15 WRK-OUT-TXMIN-RCTM  PIC  99,999        VALUE ZEROS.
                 15 FILLER              PIC  X(06)         VALUE SPACES.
                 15 WRK-OUT-TXMAX-RCTM  PIC  99,999        VALUE ZEROS.
                 15 FILLER              PIC  X(01)         VALUE SPACES.

BRQ001         10 WRK-OUT-LTBSPR-RCTM   PIC  X(17)         VALUE SPACES.
BRQ001         10 WRK-OUT-CDBSPR-ATTR-RCTM
BRQ001                                  PIC S9(04) COMP    VALUE ZEROS.
BRQ001         10 WRK-OUT-CDBSPR-RCTM.
BRQ001           15 WRK-OUT-CDBSPR-RCTM-N
BRQ001                                  PIC  9(05)         VALUE ZEROS.
BRQ001         10 WRK-OUT-DSBSPR-RCTM   PIC  X(55)         VALUE SPACES.
               10  WRK-OUT-LITAUT-RCTM  PIC  X(12)         VALUE SPACES.
               10 WRK-OUT-CODAUT-ATTR-RCTM
                                        PIC S9(004) COMP   VALUE ZEROS.
               10  WRK-OUT-CODAUT-RCTM.
                   15  WRK-OUT-CODAUT-RCTM-N
                                        PIC  ZZZZZZZZ9      VALUE ZEROS.
               10  WRK-OUT-DESAUT-ATTR-RCTM
                                        PIC S9(004) COMP    VALUE ZEROS.
               10  WRK-OUT-DESAUT-RCTM PIC  X(22)      VALUE SPACES.
               10 WRK-OUT-JAUTOR-RCTM.
                15 WRK-OUT-JAUTOR-RCTM-N PIC ZZZ.ZZZ.ZZ9,99 VALUE ZEROS.
               10 WRK-OUT-IAUTOR-RCTM.
                15 WRK-OUT-IAUTOR-RCTM-N PIC ZZZ.ZZZ.ZZ9,99 VALUE ZEROS.
               10 WRK-OUT-TACAUT-RCTM.

                15 WRK-OUT-TACAUT-RCTM-N PIC ZZZ.ZZZ.ZZ9,99 VALUE ZEROS.
               10 WRK-OUT-TOTAUT-RCTM.
                15 WRK-OUT-TOTAUT-RCTM-N PIC ZZZ.ZZZ.ZZ9,99 VALUE ZEROS.
           05  WRK-OUT-MENSA-RCTM      PIC  X(79)          VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*** AREA DE ERRO DA BRAD0660 ***'.
      *----------------------------------------------------------------*

       01  WRK-ERRO-BRAD0660.
           05  FILLER                  PIC  X(45)          VALUE
               '* RETURN-CODE DIFERENTE DE ZEROS NA BRAD0660'.
           05  FILLER                  PIC  X(12)          VALUE
               '- RET.COD ='.
           05  WRK-RETURN-CODE         PIC  9(02)          VALUE ZEROS.
           05  FILLER                  PIC  X(11)          VALUE
               ' - LOCAL ='.
           05  WRK-LOCAL-ERRO          PIC  9(02)          VALUE ZEROS.
           05  FILLER                  PIC  X(02)          VALUE ' *'.


      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*** AREA DA TELA DCOMRCTM - BRAD0660 ***'.
      *----------------------------------------------------------------*

       01  WRK-660-DCOMRCTM.
           05  WRK-660-LL-RCTM         PIC S9(004) COMP    VALUE ZEROS.
           05  WRK-660-ZZ-RCTM         PIC S9(004) COMP    VALUE ZEROS.
           05  WRK-660-SENHAS-RCTM     PIC  9(004) COMP    VALUE 0037.
           05  WRK-660-COMANDO-RCTM    PIC  9(004) COMP    VALUE 2070.
           05  WRK-660-TIPOOPC-RCTM    PIC  9(004) COMP    VALUE 0001.
           05  WRK-660-TMSTAMP-RCTM    PIC  9(004) COMP    VALUE 0026.
           05  WRK-660-PASSO-RCTM      PIC  9(004) COMP    VALUE 0001.
           05  WRK-660-CHAMADO-RCTM    PIC  9(004) COMP    VALUE 0008.
           05  WRK-660-PAGINA-RCTM     PIC  9(004) COMP    VALUE 0004.
           05  WRK-660-TPSEL-RCTM      PIC  9(004) COMP    VALUE 0001.
           05  WRK-660-CODDESC-RCTM    PIC  9(004) COMP    VALUE 0003.
           05  WRK-660-VLRIOF-RCTM     PIC  9(004) COMP    VALUE 0008.
           05  WRK-660-PAGINA1-RCTM    PIC  9(004) COMP    VALUE 0004.
           05  WRK-660-TECLPF9-RCTM    PIC  9(004) COMP    VALUE 0001.
           05  WRK-660-NOMECL-RCTM     PIC  9(004) COMP    VALUE 0040.

           05  WRK-660-CGCCPF-RCTM     PIC  9(004) COMP    VALUE 0019.
           05  WRK-660-XCONTA-RCTM     PIC  9(004) COMP    VALUE 0023.
           05  WRK-660-NUPROD-RCTM     PIC  9(004) COMP    VALUE 0003.
           05  WRK-660-NMPROD-RCTM     PIC  9(004) COMP    VALUE 0012.
           05  WRK-660-NUSPRD-RCTM     PIC  9(004) COMP    VALUE 0003.
           05  WRK-660-NMSPRD-RCTM     PIC  9(004) COMP    VALUE 0012.
           05  WRK-660-OPERAC-RCTM     PIC  9(004) COMP    VALUE 0013.
           05  WRK-660-SITUAC-RCTM     PIC  9(004) COMP    VALUE 0015.
           05  WRK-660-TITSIT-RCTM     PIC  9(004) COMP    VALUE 0013.
           05  WRK-660-DESSIT-RCTM     PIC  9(004) COMP    VALUE 0009.
           05  WRK-660-PARCEL-RCTM     PIC  9(004) COMP    VALUE 0004.
           05  WRK-660-DTVENC-RCTM     PIC  9(004) COMP    VALUE 0010.
           05  WRK-660-VALOR-RCTM      PIC  9(004) COMP    VALUE 0017.
           05  WRK-660-TAXA-RCTM       PIC  9(004) COMP    VALUE 0006.
           05  WRK-660-FREQTXOP-RCTM   PIC  9(004) COMP    VALUE 0003.
           05  WRK-660-TPJUR-RCTM      PIC  9(004) COMP    VALUE 0003.
           05  WRK-660-TSMIN-RCTM      PIC  9(004) COMP    VALUE 0018.
           05  WRK-660-TSMAX-RCTM      PIC  9(004) COMP    VALUE 0018.
           05  WRK-660-FLAG-RCTM       PIC  9(004) COMP    VALUE 0001.
           05  WRK-660-CHAMA-RCTM      PIC  9(004) COMP    VALUE 0008.
           05  WRK-660-OPER-RCTM       PIC  9(004) COMP    VALUE 0013.

           05  WRK-660-PRODIA-RCTM     PIC  9(004) COMP    VALUE 2004.
           05  WRK-660-PROMES-RCTM     PIC  9(004) COMP    VALUE 2004.
           05  WRK-660-PROANO-RCTM     PIC  9(004) COMP    VALUE 2006.
           05  WRK-660-CARTE-RCTM      PIC  9(004) COMP    VALUE 0003.
           05  WRK-660-AGCONE-RCTM     PIC  9(004) COMP    VALUE 2005.
           05  WRK-660-TXJURO-RCTM     PIC  9(004) COMP    VALUE 2011.
           05  WRK-660-FREQTX-RCTM     PIC  9(004) COMP    VALUE 2004.
BRQ001     05  WRK-660-TPJURO-RCTM     PIC  9(004) COMP    VALUE 2015.
           05  WRK-660-MINMAX-RCTM     PIC  9(004) COMP    VALUE 2027.
BRQ001     05  WRK-660-LTBSPR-RCTM     PIC  9(004) COMP    VALUE 0017.
BRQ001     05  WRK-660-CDBSPR-RCTM     PIC  9(004) COMP    VALUE 2007.
BRQ001     05  WRK-660-DSBSPR-RCTM     PIC  9(004) COMP    VALUE 0055.
           05  WRK-660-LITAUT-RCTM     PIC  9(004) COMP    VALUE 0012.
           05  WRK-660-CODAUT-RCTM     PIC  9(004) COMP    VALUE 2011.
           05  WRK-660-DESAUT-RCTM     PIC  9(004) COMP    VALUE 2024.
           05  WRK-660-JAUTOR-RCTM     PIC  9(004) COMP    VALUE 0014.
           05  WRK-660-IAUTOR-RCTM     PIC  9(004) COMP    VALUE 0014.
           05  WRK-660-TACAUT-RCTM     PIC  9(004) COMP    VALUE 0014.
           05  WRK-660-TOTAUT-RCTM     PIC  9(004) COMP    VALUE 0014.
           05  WRK-660-MENSA-RCTM      PIC  9(004) COMP    VALUE 0079.


      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*** AREA PARA MENSAGENS ***'.
      *----------------------------------------------------------------*
       COPY 'I#DCOMSY'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*** AREA DA BRAD7100 ***'.
      *----------------------------------------------------------------*
       COPY 'I#BRAD7C'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREAS DA BRAD1205 ***'.
      *----------------------------------------------------------------*

       01  WRK-MENSAGEM-1205           PIC  X(050)         VALUE SPACES.

       01  WRK-AREA-BRAD1205.
           03  WRK-DT-ENVIADA          PIC  9(008)  COMP-3 VALUE ZEROS.

           03  WRK-OPCAO               PIC  X(001)         VALUE SPACES.
           03  WRK-DT-JULIANA          PIC  9(007)  COMP-3 VALUE ZEROS.
           03  WRK-DT-EDITADA          PIC  X(010)         VALUE SPACES.
           03  WRK-DT-GREGORI          PIC  9(008)         VALUE ZEROS.
           03  WRK-DIA-SEMANA          PIC  X(013)         VALUE SPACES.
           03  WRK-MES-EXTENSO         PIC  X(009)         VALUE SPACES.
           03  WRK-DIA-UT-ANTER        PIC  9(008)         VALUE ZEROS.
           03  WRK-DIA-UT-POSTE        PIC  9(008)         VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA A BRAD0496 ***'.
      *----------------------------------------------------------------*

       01  WRK-VAL-ENTRADA             PIC  X(009)         VALUE SPACES.
       01  WRK-VAL-SAIDA               PIC  9(003)V99999   VALUE ZEROS.
       01  WRK-VAL-TAMANHO             PIC  9(008)         VALUE
           09080305.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE

           '*** DCOM1803 - FIM DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       01  LNK-IO-PCB.
           05 LNK-IO-LTERM             PIC  X(08).
           05 FILLER                   PIC  X(02).
           05 LNK-IO-STATUS            PIC  X(02).
           05 FILLER                   PIC  X(12).
           05 LNK-IO-MODNAME           PIC  X(08).

       01  LNK-ALT-PCB.
           05 LNK-ALT-LTERM            PIC  X(08).
           05 FILLER                   PIC  X(02).
           05 LNK-ALT-STATUS           PIC  X(02).
           05 FILLER                   PIC  X(12).
           05 LNK-ALT-MODNAME          PIC  X(08).


      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

           ENTRY 'DLITCBL'             USING LNK-IO-PCB
                                             LNK-ALT-PCB.

           MOVE WRK-GU                 TO WRK-FUNCAO.

      *----------------------------------------------------------------*
      *     PROCEDIMENTOS INICIAIS                                     *
      *----------------------------------------------------------------*
       0000-INICIAR                    SECTION.
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
               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE SPACES                 TO WRK-OUT-DADOS-RCTM
                                          WRK-OUT-SENHAS-RCTM
                                          WRK-OUT-COMANDO-RCTM.

           PERFORM 2000-PROCESSAR-MENSAGEM.

           GO TO 0000-INICIAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     PROCESSAR MENSAGEM                                         *

      *----------------------------------------------------------------*
       2000-PROCESSAR-MENSAGEM         SECTION.
      *----------------------------------------------------------------*


           MOVE 'DCOMRCTM'             TO WRK-TELA

           IF  WRK-TRANSACAO           NOT EQUAL 'DCOM1803'
               EVALUATE  WRK-TRANSACAO
                   WHEN 'DCOM0105'
                         PERFORM 2110-RECEBER-CONTROLE-2
BRQ001
BRQ001             WHEN 'DCOM3929'
BRQ001                   PERFORM 2160-RECEBER-CTRL-3929
BRQ001
                   WHEN  OTHER
                         PERFORM 2100-RECEBER-CONTROLE
               END-EVALUATE
           ELSE
               PERFORM 2200-PROCESSAR-DCOMRCTM
           END-IF.


           IF  WRK-TELA                EQUAL 'DCOMRCTM'
               PERFORM 2300-PROCESSAR-BRAD0660
           END-IF.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     RECEBER CONTROLE                                           *
      *----------------------------------------------------------------*
       2100-RECEBER-CONTROLE           SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-MENSAGEM             TO WRK-COMU-AREA.

           MOVE 'DCOM7999'       TO WRK-MODULO

           CALL WRK-MODULO USING WRK-TIMESTAMP
                                 ERRO-AREA

                                 WRK-SQLCA.

           MOVE WRK-TIMESTAMP          TO WRK-INP-TMSTAMP-RCTM
                                          WRK-OUT-TMSTAMP-RCTM.

           MOVE WRK-COMU-TRANSACAO       TO WRK-INP-CHAMADO-RCTM
                                            WRK-OUT-CHAMADO-RCTM

           MOVE WRK-COMU-CHAMADOR        TO WRK-INP-CHAMA-RCTM
                                            WRK-OUT-CHAMA-RCTM

           MOVE WRK-COMU-DANO-OPER       TO WRK-INP-OPERAC-RCTM(1:4)
                                            WRK-OUT-OPERAC-RCTM(1:4)
           MOVE WRK-COMU-NSEQ-OPER       TO WRK-INP-OPERAC-RCTM(5:9)
                                            WRK-OUT-OPERAC-RCTM(5:9)
           MOVE WRK-COMU-OPCAO           TO WRK-INP-TIPOOPC-RCTM
                                            WRK-OUT-TIPOOPC-RCTM
           MOVE WRK-COMU-TP-SELECAO      TO WRK-INP-TPSEL-RCTM
                                            WRK-OUT-TPSEL-RCTM

           MOVE WRK-COMU-PAGINA-2        TO WRK-INP-PAGINA-RCTM

                                            WRK-OUT-PAGINA-RCTM

           MOVE WRK-COMU-PAGINA-1        TO WRK-INP-PAGINA1-RCTM
                                            WRK-OUT-PAGINA1-RCTM

           MOVE ZEROS                    TO WRK-TXJURM
                                            WRK-TXJURA-4174
                                            WRK-TXJURM-4174.

           PERFORM 2105-CARREGA-CABECALHO

           IF  WRK-OUT-TPJURO-RCTM          EQUAL 'TXREDE' OR 'TXAESP'
               IF  WRK-TXJURM         LESS  4174-SAI-PMAX-TX-JURO-REGRA
                   MOVE 'TXAESP'            TO WRK-INP-TPJURO-RCTM
                                               WRK-OUT-TPJURO-RCTM
               ELSE
                   MOVE 'TXREDE'            TO WRK-INP-TPJURO-RCTM
                                               WRK-OUT-TPJURO-RCTM
               END-IF
           END-IF


           INITIALIZE                  WRK-5475-ENTRADA
                                       WRK-5475-SAIDA.

           MOVE WRK-COD-USER-R         TO WRK-5475E-CFUNC-BDSCO
           MOVE LNK-IO-LTERM           TO WRK-5475E-CTERM
           MOVE 'IP'                   TO WRK-5475E-FLAG-FILTRO
           MOVE WRK-COMU-OPERACAO      TO WRK-5475E-OPERACAO
           MOVE WRK-COMU-PARCELA       TO WRK-5475E-NPCELA-DESC-COML


           MOVE 'DCOM5475'             TO WRK-MODULO

           CALL WRK-MODULO             USING WRK-5475-ENTRADA
                                             WRK-5475-SAIDA
                                             ERRO-AREA
                                             WRK-SQLCA.


           EVALUATE WRK-5475S-COD-RETORNO
              WHEN '0000'
                    PERFORM 2130-FORMATAR-TELA

                    MOVE 1                      TO WRK-OUT-PASSO-RCTM
                    MOVE SG-MENSAGEM(0014)      TO WRK-OUT-MENSA-RCTM

              WHEN '0099'
                    MOVE 'DB2'         TO ERR-TIPO-ACESSO
                    PERFORM 9999-PROCESSAR-ROTINA-ERRO

              WHEN OTHER
                   MOVE WRK-5475S-MSG-RETORNO
                                       TO WRK-COMU-MENSAGEM
                   MOVE '*'            TO WRK-INCONSIS
                   PERFORM 2220-RETORNAR-CHAMADOR
                   GO TO 2100-99-FIM

           END-EVALUATE.

BRQ001     IF  4174-SAI-CTPO-DESC-COML EQUAL 007 OR 008
BRQ001         IF  WRK-5475S-REGISTRAR-BACEN EQUAL 'S'
BRQ001             MOVE 'BASE PRORROGACAO:'
BRQ001                                 TO WRK-INP-LTBSPR-RCTM
BRQ001                                    WRK-OUT-LTBSPR-RCTM

BRQ001             MOVE WRK-209        TO WRK-OUT-CDBSPR-ATTR-RCTM
BRQ001         END-IF
BRQ001     END-IF.
BRQ001
           PERFORM 2285-ACESSAR-DCOM5424


           MOVE WRK-OUT-TXJURO-RCTM-N  TO WRK-TAXA-1.
           MOVE WRK-OUT-TAXA-RCTM      TO WRK-TAXA-2.

           IF WRK-TAXA-1               EQUAL WRK-TAXA-2
              MOVE WRK-229             TO WRK-OUT-MINMAX-ATTR-RCTM
           END-IF.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA CARREGA OS CAMPOS DO PROGRAMA DCOM0105            *
      *----------------------------------------------------------------*

       2110-RECEBER-CONTROLE-2         SECTION.
      *----------------------------------------------------------------*


           MOVE WRK-MENSAGEM              TO  COMU-AREA-0105

           MOVE  COMU-TMSTAMP-0105         TO  WRK-INP-TMSTAMP-RCTM
                                               WRK-OUT-TMSTAMP-RCTM

           PERFORM 1151-LER-DBTEMP.

           MOVE WRK-SAI-DADOS          TO WRK-INPUT-RCTM.

           PERFORM 1152-DEL-DBTEMP.

           PERFORM 2210-DEVOLVER-TELA.

           MOVE  COMU-CCART-0105           TO  WRK-OUT-CARTE-RCTM
           MOVE  COMU-NRCOUM-0105          TO  WRK-OUT-OPER-RCTM(1:4)
           MOVE  COMU-NRCODOIS-0105        TO  WRK-OUT-AGCONE-RCTM
           MOVE  COMU-NRCOTRES-0105        TO  WRK-OUT-OPER-RCTM(5:9)


           MOVE  'S'                       TO  WRK-OUT-TECLPF9-RCTM

           MOVE  2    TO WRK-OUT-PASSO-RCTM
           MOVE SPACES     TO WRK-INCONSIS

           IF  COMU-DSC-MSG-0105     NOT EQUAL SPACES
               MOVE COMU-DSC-MSG-0105          TO WRK-OUT-MENSA-RCTM
           ELSE
               IF  COMU-CSIT-CONS-NEGOC-0105     EQUAL 95 OR 98 OR 00
                   MOVE 'CONSULTA CONE PENDENTE DE AUTORIZ. TECLE <PF9>
      -                 'PARA NOVA CONSULTA'   TO WRK-OUT-MENSA-RCTM
                   MOVE 1                      TO WRK-OUT-PASSO-RCTM
                   MOVE 'N'                    TO WRK-OUT-TECLPF9-RCTM
BRQ001             IF WRK-INP-LTBSPR-RCTM NOT EQUAL SPACES
BRQ001                MOVE WRK-209        TO WRK-OUT-CDBSPR-ATTR-RCTM
BRQ001             END-IF
                   GO TO 2110-99-FIM
               END-IF
               IF  COMU-DVCTO-LMTE-0105  NOT EQUAL SPACES
                   MOVE  'S'                   TO  WRK-OUT-TECLPF9-RCTM


                   PERFORM 2283-CONSISTIR-TXJURO-MENSAL
                   PERFORM 2282-OBTER-TOTAIS

                     MOVE WRK-PNNS        TO  WRK-OUT-TXJURO-ATTR-RCTM
                     MOVE WRK-PNNS        TO  WRK-OUT-PRODIA-ATTR-RCTM
                     MOVE WRK-PNNS        TO  WRK-OUT-PROMES-ATTR-RCTM
                     MOVE WRK-PNNS        TO  WRK-OUT-PROANO-ATTR-RCTM

BRQ001             IF WRK-INP-LTBSPR-RCTM NOT EQUAL SPACES
BRQ001                MOVE WRK-241        TO WRK-OUT-CDBSPR-ATTR-RCTM
BRQ001             END-IF

                   MOVE 'TECLE <PF6> PARA CONFIRMAR OU <ENTER> PARA ALTE
      -                 'RAR'                  TO WRK-OUT-MENSA-RCTM
                   MOVE '2'                    TO WRK-OUT-PASSO-RCTM

               ELSE
                   MOVE WRK-49361           TO WRK-OUT-COMANDO-ATTR-RCTM
                   MOVE 3                      TO WRK-OUT-PASSO-RCTM
BRQ001             IF WRK-INP-LTBSPR-RCTM NOT EQUAL SPACES

BRQ001                MOVE WRK-241        TO WRK-OUT-CDBSPR-ATTR-RCTM
BRQ001             END-IF
                   MOVE 'TECLE <PF9> PARA CONSULTAR CREDITO OU <ENTER> P
      -                 'ARA ALTERAR'
                                               TO WRK-OUT-MENSA-RCTM
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2105-CARREGA-CABECALHO          SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                     4174-ENTRADA-ROTEADOR
                                          4174-SAIDA-ROTEADOR

           MOVE '0000'                 TO 4174-ENT-COD-RETORNO
           MOVE SPACES                 TO 4174-ENT-MSG-RETORNO

           MOVE ZEROS                  TO 4174-ENT-RESTART
                                          4174-ENT-FLAG
           MOVE WRK-COD-USER           TO 4174-ENT-CFUNC-BDSCO
           MOVE LNK-IO-LTERM           TO 4174-ENT-CTERM
           MOVE WRK-COMU-DANO-OPER     TO 4174-ENT-DANO-OPER-DESC
           MOVE WRK-COMU-NSEQ-OPER     TO 4174-ENT-NSEQ-OPER-DESC

           MOVE 'DCOM4174'             TO WRK-MODULO

           CALL WRK-MODULO          USING 4174-ENTRADA-ROTEADOR
                                          4174-SAIDA-ROTEADOR
                                          ERRO-AREA
                                          WRK-SQLCA.

           EVALUATE 4174-SAI-COD-RETORNO

              WHEN '0000'
               PERFORM 2107-FORMATA-CABECALHO

              WHEN '0099'
                    MOVE 'DB2'         TO ERR-TIPO-ACESSO

                    PERFORM 9999-PROCESSAR-ROTINA-ERRO

              WHEN OTHER
                   MOVE 4174-SAI-MSG-RETORNO
                                       TO WRK-COMU-MENSAGEM
                   MOVE '*'            TO WRK-INCONSIS
                   PERFORM 2220-RETORNAR-CHAMADOR

           END-EVALUATE.


      *----------------------------------------------------------------*
       2105-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2107-FORMATA-CABECALHO          SECTION.
      *----------------------------------------------------------------*

           MOVE 4174-SAI-IPSSOA-DESC-COML  TO WRK-OUT-NOMECL-RCTM
           MOVE 4174-SAI-CPRODT            TO WRK-INP-CODPRD-RCTM

                                              WRK-OUT-CODPRD-RCTM
           MOVE 4174-SAI-IABREV-PRODT      TO WRK-OUT-DESPRD-RCTM
           MOVE 4174-SAI-CSPROD-DESC-COML  TO WRK-INP-CODSUB-RCTM
                                              WRK-OUT-CODSUB-RCTM
           MOVE 4174-SAI-CTPO-DESC-COML    TO WRK-OUT-CODDESC-RCTM
           MOVE 4174-SAI-IRSUMO-SPROD-DESC TO WRK-OUT-DESSUB-RCTM
           MOVE 4174-SAI-CSIT-DESC-COML    TO WRK-OUT-SITUAC-RCTM
           STRING 4174-SAI-CCNPJ-CPF(1:3)
           '.'    4174-SAI-CCNPJ-CPF(4:3)
           '.'    4174-SAI-CCNPJ-CPF(7:3)
           '/'    4174-SAI-CFLIAL-CNPJ
           '-'    4174-SAI-CCTRL-CNPJ-CPF
           DELIMITED BY SIZE             INTO WRK-OUT-CGCCPF-RCTM
           MOVE 4174-SAI-CBCO              TO WRK-BANCO-AUX
           MOVE 4174-SAI-CAG-BCRIA         TO WRK-AGENC-AUX
           MOVE 4174-SAI-CCTA-BCRIA-CLI    TO WRK-CONTA-AUX
           MOVE WRK-BCOAGECTA-AUX          TO WRK-OUT-XCONTA-RCTM
           MOVE 4174-SAI-IRSUMO-SIT-DESC   TO WRK-OUT-SITUAC-RCTM
           MOVE 4174-SAI-CSGL-TPO-TX-JURO  TO WRK-INP-TPJURO-RCTM
                                              WRK-OUT-TPJURO-RCTM
           MOVE 4174-SAI-CTPO-TX-JURO-DESC  TO WRK-OUT-TPJUR-RCTM.


           MOVE 4174-SAI-PTX-JURO-ANO       TO WRK-TXJURA-4174
           MOVE 4174-SAI-PTX-JURO-MES       TO WRK-TXJURM-4174

           IF  4174-SAI-CINDCD-FREQ-TX      EQUAL 'AA'
               MOVE 4174-SAI-PTX-JURO-ANO   TO WRK-OUT-TXJURO-RCTM-N
                                               WRK-OUT-TAXA-RCTM
                                               WRK-TXJURM
               MOVE 'A.A.'                  TO WRK-INP-FREQTX-RCTM
                                               WRK-OUT-FREQTX-RCTM
                                               WRK-OUT-FREQTXOP-RCTM
           ELSE
               MOVE WRK-TXJURM-4174         TO WRK-OUT-TXJURO-RCTM-N
                                               WRK-OUT-TAXA-RCTM
                                               WRK-TXJURM
               MOVE 'A.M.'                  TO WRK-INP-FREQTX-RCTM
                                               WRK-OUT-FREQTX-RCTM
                                               WRK-OUT-FREQTXOP-RCTM
           END-IF

           IF  4174-SAI-CTPO-TX-JURO-DESC   EQUAL 003

               MOVE WRK-241                 TO WRK-OUT-TXJURO-ATTR-RCTM
           END-IF.
           MOVE 4174-SAI-PALIQT-IOF-DESC    TO WRK-INP-VLRIOF-RCTM
                                               WRK-OUT-VLRIOF-RCTM.
           MOVE 4174-SAI-CSGL-TPO-TX-JURO   TO WRK-INP-TPJURO-RCTM
                                               WRK-OUT-TPJURO-RCTM.


           MOVE 4174-SAI-PMAX-TX-JURO-REGRA TO WRK-TAX-MAX
           MOVE 4174-SAI-PMIN-TX-JURO-REGRA TO WRK-TAX-MIN


           IF  4174-SAI-CTPO-TX-JURO-DESC   NOT EQUAL 003
               MOVE WRK-TAXA-MINMAX             TO WRK-INP-MINMAX-RCTM
                                                   WRK-OUT-MINMAX-RCTM
           END-IF

           IF  4174-SAI-CCONTR-CONVE-DESC  GREATER ZEROS
               IF  4174-SAI-CELMTO-DESC-COML  EQUAL 9
                   MOVE 'CONV CLIENTE:'       TO WRK-OUT-DESIDE-RCTM
               ELSE

                   MOVE 'CONV GERAL  :'       TO WRK-OUT-DESIDE-RCTM
               END-IF
               MOVE 4174-SAI-CCONTR-CONVE-DESC
                                            TO WRK-OUT-NUMERO-RCTM
           ELSE
              IF 4174-SAI-CCONTR-LIM-DESC   GREATER ZEROS
                 MOVE 'CONTR LIMITE:'       TO WRK-OUT-DESIDE-RCTM
                 MOVE 4174-SAI-CCONTR-LIM-DESC
                                            TO WRK-OUT-NUMERO-RCTM
              ELSE
                 IF 4174-SAI-NSMULA-OPER-DESC GREATER ZEROS
                      MOVE 'SIMULACAO:'     TO WRK-OUT-DESIDE-RCTM
                      MOVE 4174-SAI-NSMULA-OPER-DESC
                                           TO WRK-OUT-NUMERO-RCTM
                 END-IF
              END-IF
           END-IF.

      *----------------------------------------------------------------*
       2107-99-FIM.                    EXIT.
      *----------------------------------------------------------------*


      *----------------------------------------------------------------*
      *     FORMATAR A TELA                                            *
      *----------------------------------------------------------------*
       2130-FORMATAR-TELA              SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-5475E-NPCELA-DESC-COML
                                       TO WRK-OUT-PARCEL-RCTM

           INSPECT WRK-5475S-DVCTO-PCELA-DESC-DE
           REPLACING ALL '.' BY '/'

           MOVE WRK-5475S-DVCTO-PCELA-DESC-DE
                                       TO WRK-OUT-DTVENC-RCTM
           MOVE WRK-5475S-VALOR-PCELA-DE
                                       TO WRK-OUT-VALOR-RCTM

           MOVE SPACES                 TO WRK-OUT-PRODIA-RCTM
                                          WRK-OUT-PROMES-RCTM
                                          WRK-OUT-PROANO-RCTM

           .
      *----------------------------------------------------------------*
       2130-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA EFETUA A LEITURA DA TABELA TEMPORARIA .           *
      *----------------------------------------------------------------*
       1151-LER-DBTEMP                 SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WRK-ENTRADA-DCOM7995
                      WRK-SAIDA-DCOM7995.

           MOVE 'CS'                   TO WRK-ENT-OPCAO
           MOVE WRK-COD-USER-R         TO WRK-ENT-COD-FUNC
           MOVE LNK-IO-LTERM           TO WRK-ENT-COD-TERMINAL
           MOVE WRK-INP-TMSTAMP-RCTM   TO WRK-ENT-TIMESTAMP
           MOVE 'DCOM1803'             TO WRK-ENT-TRANS
           MOVE SPACES                 TO WRK-ENT-TRANS-ANT
           MOVE 1                      TO WRK-ENT-PAGINA


           MOVE 'DCOM7995'             TO WRK-MODULO

           CALL WRK-MODULO             USING WRK-ENTRADA-DCOM7995
                                             WRK-SAIDA-DCOM7995
                                             ERRO-AREA
                                             WRK-SQLCA.

           IF  WRK-SAI-COD-RETORNO     EQUAL 01
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE WRK-SAI-MSG-RETORNO
                                       TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           ELSE
               IF  WRK-SAI-COD-RETORNO EQUAL 99
                   MOVE 'DB2'          TO ERR-TIPO-ACESSO
                   PERFORM 9999-PROCESSAR-ROTINA-ERRO
               END-IF
           END-IF.

      *----------------------------------------------------------------*

       1151-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *  ESSA ROTINA EFETUA O TRATAMENTO DE RETORNO AO MENU ROTINAS    *
      *----------------------------------------------------------------*
       1152-DEL-DBTEMP                 SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WRK-ENTRADA-DCOM7995
                      WRK-SAIDA-DCOM7995.

           MOVE 'ET'                   TO WRK-ENT-OPCAO
           MOVE WRK-COD-USER-R         TO WRK-ENT-COD-FUNC
           MOVE LNK-IO-LTERM           TO WRK-ENT-COD-TERMINAL
           MOVE WRK-INP-TMSTAMP-RCTM   TO WRK-ENT-TIMESTAMP
           MOVE 'DCOM1803'             TO WRK-ENT-TRANS
           MOVE SPACES                 TO WRK-ENT-TRANS-ANT
           MOVE 1                      TO WRK-ENT-PAGINA

           MOVE 'DCOM7995'             TO WRK-MODULO


           CALL WRK-MODULO             USING WRK-ENTRADA-DCOM7995
                                             WRK-SAIDA-DCOM7995
                                             ERRO-AREA
                                             WRK-SQLCA.

           IF  WRK-SAI-COD-RETORNO     EQUAL 01
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE WRK-SAI-MSG-RETORNO
                                       TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           ELSE
               IF  WRK-SAI-COD-RETORNO EQUAL 99
                   MOVE 'DB2'          TO ERR-TIPO-ACESSO
                   PERFORM 9999-PROCESSAR-ROTINA-ERRO
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       1152-99-FIM.                    EXIT.
      *----------------------------------------------------------------*


BRQ001*----------------------------------------------------------------*
BRQ001*     RECEBER CONTROLE DO PROCESSAMENTO DA TRANSACAO DCOM3929    *
BRQ001*----------------------------------------------------------------*
BRQ001 2160-RECEBER-CTRL-3929          SECTION.
BRQ001*----------------------------------------------------------------*
BRQ001
BRQ001     MOVE WRK-MENSAGEM           TO WRK-COMU-AREA-3929.
BRQ001     MOVE WRK-COMU-TMSTAMP-3929  TO WRK-INP-TMSTAMP-RCTM
BRQ001                                    WRK-OUT-TMSTAMP-RCTM.
BRQ001
BRQ001     PERFORM 1151-LER-DBTEMP.
BRQ001
BRQ001     MOVE WRK-SAI-DADOS          TO WRK-INPUT-RCTM.
BRQ001
BRQ001     PERFORM 1152-DEL-DBTEMP.
BRQ001
BRQ001     PERFORM 2210-DEVOLVER-TELA.
BRQ001
BRQ001     PERFORM 2262-TRATAR-ATRIB-4473.
BRQ001
BRQ001     IF  WRK-COMU-CDBASE-N       EQUAL ZEROS

BRQ001         MOVE WRK-209            TO WRK-OUT-CDBSPR-ATTR-RCTM
BRQ001     ELSE
BRQ001         MOVE WRK-COMU-CDBASE-N  TO WRK-OUT-CDBSPR-RCTM-N
BRQ001         STRING ' - '
BRQ001              WRK-COMU-RBASE(1:52)
BRQ001         DELIMITED BY SIZE     INTO WRK-OUT-DSBSPR-RCTM
BRQ001         MOVE WRK-49361          TO WRK-OUT-CDBSPR-ATTR-RCTM
BRQ001     END-IF.
BRQ001
BRQ001     IF  WRK-COMU-MENSAGEM-3929  NOT EQUAL SPACES
BRQ001         MOVE WRK-COMU-MENSAGEM-3929 TO WRK-OUT-MENSA-RCTM
BRQ001     END-IF.
BRQ001
BRQ001     IF  WRK-INP-LITAUT-RCTM     EQUAL 'COD.AUTORIZ:'
BRQ001         MOVE WRK-209            TO WRK-OUT-CODAUT-ATTR-RCTM
BRQ001     END-IF.
BRQ001
BRQ001*----------------------------------------------------------------*
BRQ001 2160-99-FIM.                    EXIT.
BRQ001*----------------------------------------------------------------*


      *----------------------------------------------------------------*
      *     PROCESSAR DCOMRCTM                                         *
      *----------------------------------------------------------------*
       2200-PROCESSAR-DCOMRCTM         SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-MENSAGEM           TO WRK-INPUT-RCTM.

           PERFORM 2210-DEVOLVER-TELA

           PERFORM 2262-TRATAR-ATRIB-4473

           EVALUATE TRUE
               WHEN WRK-INP-PFK-RCTM        EQUAL 'H' OR
                   (WRK-INP-PFK-RCTM        EQUAL '.' AND
                    WRK-INP-COMANDO-RCTM    EQUAL 'PFK01')
                    PERFORM 2262-TRATAR-ATRIB-4473
                    MOVE WRK-INP-MENSA-RCTM TO WRK-OUT-MENSA-RCTM
                    IF  WRK-INP-PASSO-RCTM  EQUAL '2' OR '3'
                        MOVE WRK-49361     TO WRK-OUT-COMANDO-ATTR-RCTM
                        MOVE WRK-PNNS-C    TO  WRK-OUT-TXJURO-ATTR-RCTM

                        MOVE WRK-PNNS      TO  WRK-OUT-PRODIA-ATTR-RCTM
                        MOVE WRK-PNNS      TO  WRK-OUT-PROMES-ATTR-RCTM
                        MOVE WRK-PNNS      TO  WRK-OUT-PROANO-ATTR-RCTM
                    END-IF

               WHEN WRK-INP-PFK-RCTM      EQUAL '3' OR
                   (WRK-INP-PFK-RCTM      EQUAL '.' AND
                    WRK-INP-COMANDO-RCTM  EQUAL 'PFK03')
                    PERFORM 2220-RETORNAR-CHAMADOR

               WHEN WRK-INP-PFK-RCTM      EQUAL 'X' OR
                   (WRK-INP-PFK-RCTM      EQUAL '.' AND
                    WRK-INP-COMANDO-RCTM  EQUAL 'PFK05')
                    PERFORM 2230-MENU-ROTINAS

               WHEN WRK-INP-PFK-RCTM      EQUAL 'Z' OR
                   (WRK-INP-PFK-RCTM      EQUAL '.' AND
                    WRK-INP-COMANDO-RCTM  EQUAL 'PFK10')
                    PERFORM 2270-MENU-DCOM

               WHEN  WRK-INP-PASSO-RCTM   EQUAL '1'

                     PERFORM 2201-TRATAR-PASSO-1

               WHEN  WRK-INP-PASSO-RCTM   EQUAL '2'
                     PERFORM 2202-TRATAR-PASSO-2

               WHEN  WRK-INP-PASSO-RCTM   EQUAL '3'
                     PERFORM 2203-TRATAR-PASSO-3

               WHEN  OTHER
                    IF  WRK-INP-PASSO-RCTM  EQUAL '2' OR '3'
                        MOVE WRK-49361     TO WRK-OUT-COMANDO-ATTR-RCTM
                        MOVE WRK-PNNS-C    TO  WRK-OUT-TXJURO-ATTR-RCTM
                        MOVE WRK-PNNS      TO  WRK-OUT-PRODIA-ATTR-RCTM
                        MOVE WRK-PNNS      TO  WRK-OUT-PROMES-ATTR-RCTM
                        MOVE WRK-PNNS      TO  WRK-OUT-PROANO-ATTR-RCTM
                    END-IF
                    IF WRK-INP-LITAUT-RCTM EQUAL 'COD.AUTORIZ:'
                       MOVE WRK-209        TO WRK-OUT-CODAUT-ATTR-RCTM
                    END-IF
BRQ001              IF WRK-INP-LTBSPR-RCTM NOT EQUAL SPACES
BRQ001                 MOVE WRK-209        TO WRK-OUT-CDBSPR-ATTR-RCTM

BRQ001              END-IF
                    MOVE 'PF INVALIDA'
                                      TO  WRK-OUT-MENSA-RCTM

           END-EVALUATE.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2201-TRATAR-PASSO-1             SECTION.
      *----------------------------------------------------------------*

           EVALUATE TRUE
               WHEN (WRK-INP-PFK-RCTM     EQUAL '.' AND
                     WRK-INP-COMANDO-RCTM EQUAL SPACES)
                     PERFORM 2280-TRATAR-ENTER

               WHEN  OTHER
BRQ001               IF  WRK-INP-PFK-RCTM      EQUAL '9' OR

BRQ001                  (WRK-INP-PFK-RCTM      EQUAL '.' AND
BRQ001                   WRK-INP-COMANDO-RCTM  EQUAL 'PFK09')
BRQ001                   IF WRK-INP-LINHA-RCTM EQUAL   16 AND
BRQ001                     (WRK-INP-COLUN-RCTM GREATER 19 AND
BRQ001                      WRK-INP-COLUN-RCTM LESS    25)
BRQ001                      PERFORM 2290-ACESSAR-DCOM3929
BRQ001                      GO TO 2201-99-FIM
BRQ001                   END-IF
BRQ001              END-IF
BRQ001
                    IF  WRK-INP-PASSO-RCTM  EQUAL '2' OR '3'
                        MOVE WRK-49361     TO WRK-OUT-COMANDO-ATTR-RCTM
                        MOVE WRK-PNNS-C    TO  WRK-OUT-TXJURO-ATTR-RCTM
                        MOVE WRK-PNNS      TO  WRK-OUT-PRODIA-ATTR-RCTM
                        MOVE WRK-PNNS      TO  WRK-OUT-PROMES-ATTR-RCTM
                        MOVE WRK-PNNS      TO  WRK-OUT-PROANO-ATTR-RCTM
                    END-IF
                    IF WRK-INP-LITAUT-RCTM EQUAL 'COD.AUTORIZ:'
                       MOVE WRK-209        TO WRK-OUT-CODAUT-ATTR-RCTM
                    END-IF
BRQ001              IF WRK-INP-LTBSPR-RCTM NOT EQUAL SPACES

BRQ001                 MOVE WRK-209        TO WRK-OUT-CDBSPR-ATTR-RCTM
BRQ001              END-IF
                    MOVE 'PF INVALIDA'
                                      TO  WRK-OUT-MENSA-RCTM

           END-EVALUATE.

      *----------------------------------------------------------------*
       2201-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2202-TRATAR-PASSO-2             SECTION.
      *----------------------------------------------------------------*

           EVALUATE TRUE
               WHEN WRK-INP-PFK-RCTM      EQUAL '6' OR
                   (WRK-INP-PFK-RCTM      EQUAL '.' AND
                    WRK-INP-COMANDO-RCTM  EQUAL 'PFK06')
                     PERFORM 2280-50-ACESSAR-DCOM5335
                     IF  WRK-INCONSIS     EQUAL SPACES

                         INITIALIZE WRK-5470-ENTRADA
                                    WRK-5470-SAIDA
                         MOVE SPACES       TO WRK-5470E-FLAG-CONSIST
                         PERFORM 2250-CONFIRMA
                         IF  WRK-INCONSIS  EQUAL SPACES
                             MOVE WRK-PNNS TO WRK-OUT-TXJURO-ATTR-RCTM
                             MOVE WRK-PNNS TO WRK-OUT-PRODIA-ATTR-RCTM
                             MOVE WRK-PNNS TO WRK-OUT-PROMES-ATTR-RCTM
                             MOVE WRK-PNNS TO WRK-OUT-PROANO-ATTR-RCTM
                         END-IF
                     ELSE
                         PERFORM 2262-TRATAR-ATRIB-4473
                         MOVE '1'         TO WRK-OUT-PASSO-RCTM
                         MOVE WRK-UNNS-C  TO WRK-OUT-PRODIA-ATTR-RCTM
BRQ001                   IF WRK-INP-LTBSPR-RCTM NOT EQUAL SPACES
BRQ001                      MOVE WRK-209  TO WRK-OUT-CDBSPR-ATTR-RCTM
BRQ001                   END-IF
                     END-IF

               WHEN (WRK-INP-PFK-RCTM     EQUAL '.' AND
                     WRK-INP-COMANDO-RCTM EQUAL SPACES)

                     PERFORM 2262-TRATAR-ATRIB-4473
                     MOVE SG-MENSAGEM(0014) TO WRK-OUT-MENSA-RCTM
                     MOVE '1'             TO  WRK-OUT-PASSO-RCTM
                     MOVE WRK-UNNS-C      TO  WRK-OUT-PRODIA-ATTR-RCTM
                     MOVE SPACES          TO  WRK-OUT-JAUTOR-RCTM
                                              WRK-OUT-IAUTOR-RCTM
                                              WRK-OUT-TACAUT-RCTM
                                              WRK-OUT-TOTAUT-RCTM
                                              WRK-OUT-AGCONE-RCTM-X
                     IF WRK-INP-LITAUT-RCTM EQUAL 'COD.AUTORIZ:'
                        MOVE WRK-49361      TO WRK-OUT-CODAUT-ATTR-RCTM
                     END-IF
BRQ001               IF WRK-INP-LTBSPR-RCTM NOT EQUAL SPACES
BRQ001                  MOVE WRK-209        TO WRK-OUT-CDBSPR-ATTR-RCTM
BRQ001               END-IF

               WHEN  OTHER
                     MOVE WRK-49361       TO WRK-OUT-COMANDO-ATTR-RCTM
                     MOVE WRK-PNNS        TO  WRK-OUT-TXJURO-ATTR-RCTM
                     MOVE WRK-PNNS        TO  WRK-OUT-PRODIA-ATTR-RCTM
                     MOVE WRK-PNNS        TO  WRK-OUT-PROMES-ATTR-RCTM

                     MOVE WRK-PNNS        TO  WRK-OUT-PROANO-ATTR-RCTM
                     IF WRK-INP-LITAUT-RCTM EQUAL 'COD.AUTORIZ:'
                        MOVE WRK-209        TO WRK-OUT-CODAUT-ATTR-RCTM
                     END-IF
BRQ001               IF WRK-INP-LTBSPR-RCTM NOT EQUAL SPACES
BRQ001                  MOVE WRK-241        TO WRK-OUT-CDBSPR-ATTR-RCTM
BRQ001               END-IF
                     MOVE 'PF INVALIDA'
                                       TO  WRK-OUT-MENSA-RCTM

           END-EVALUATE.

      *----------------------------------------------------------------*
       2202-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2203-TRATAR-PASSO-3             SECTION.
      *----------------------------------------------------------------*

           EVALUATE TRUE

               WHEN WRK-INP-PFK-RCTM     EQUAL '9' OR
                   (WRK-INP-PFK-RCTM     EQUAL '.' AND
                    WRK-INP-COMANDO-RCTM EQUAL 'PFK09')
                    PERFORM 2260-TRATA-CONSULTA

               WHEN (WRK-INP-PFK-RCTM     EQUAL '.' AND
                     WRK-INP-COMANDO-RCTM EQUAL SPACES)
BRQ001              IF WRK-INP-LTBSPR-RCTM NOT EQUAL SPACES
BRQ001                 MOVE WRK-209        TO WRK-OUT-CDBSPR-ATTR-RCTM
BRQ001              END-IF
                    MOVE '1'          TO WRK-OUT-PASSO-RCTM
                    MOVE 'PREENCHA OS DADOS E TECLE <ENTER>'
                                      TO WRK-OUT-MENSA-RCTM

               WHEN  OTHER
                     MOVE WRK-49361       TO WRK-OUT-COMANDO-ATTR-RCTM
                     MOVE WRK-PNNS        TO  WRK-OUT-TXJURO-ATTR-RCTM
                     MOVE WRK-PNNS        TO  WRK-OUT-PRODIA-ATTR-RCTM
                     MOVE WRK-PNNS        TO  WRK-OUT-PROMES-ATTR-RCTM
                     MOVE WRK-PNNS        TO  WRK-OUT-PROANO-ATTR-RCTM
                     IF WRK-INP-LITAUT-RCTM EQUAL 'COD.AUTORIZ:'

                        MOVE WRK-209        TO WRK-OUT-CODAUT-ATTR-RCTM
                     END-IF
BRQ001               IF WRK-INP-LTBSPR-RCTM NOT EQUAL SPACES
BRQ001                  MOVE WRK-241        TO WRK-OUT-CDBSPR-ATTR-RCTM
BRQ001               END-IF
                     MOVE 'PF INVALIDA'
                                       TO  WRK-OUT-MENSA-RCTM

           END-EVALUATE.

      *----------------------------------------------------------------*
       2203-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     DEVOLVER TELA                                              *
      *----------------------------------------------------------------*
       2210-DEVOLVER-TELA              SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-INP-SENHAS-RCTM    TO WRK-OUT-SENHAS-RCTM

           MOVE WRK-INP-COMANDO-RCTM   TO WRK-OUT-COMANDO-RCTM
           MOVE WRK-INP-TIPOOPC-RCTM   TO WRK-OUT-TIPOOPC-RCTM
           MOVE WRK-INP-TPSEL-RCTM     TO WRK-OUT-TPSEL-RCTM
           MOVE WRK-INP-CODDESC-RCTM   TO WRK-OUT-CODDESC-RCTM
           MOVE WRK-INP-VLRIOF-RCTM    TO WRK-OUT-VLRIOF-RCTM
           MOVE WRK-INP-TMSTAMP-RCTM   TO WRK-OUT-TMSTAMP-RCTM
           MOVE WRK-INP-PASSO-RCTM     TO WRK-OUT-PASSO-RCTM
           MOVE WRK-INP-CHAMADO-RCTM   TO WRK-OUT-CHAMADO-RCTM
           MOVE WRK-INP-CHAMA-RCTM     TO WRK-OUT-CHAMA-RCTM
           MOVE WRK-INP-NOMECL-RCTM    TO WRK-OUT-NOMECL-RCTM
           MOVE WRK-INP-CGCCPF-RCTM    TO WRK-OUT-CGCCPF-RCTM
           MOVE WRK-INP-XCONTA-RCTM    TO WRK-OUT-XCONTA-RCTM
           MOVE WRK-INP-CODPRD-RCTM    TO WRK-OUT-CODPRD-RCTM
           MOVE WRK-INP-DESPRD-RCTM    TO WRK-OUT-DESPRD-RCTM
           MOVE WRK-INP-CODSUB-RCTM    TO WRK-OUT-CODSUB-RCTM
           MOVE WRK-INP-DESSUB-RCTM    TO WRK-OUT-DESSUB-RCTM
           MOVE WRK-INP-OPERAC-RCTM    TO WRK-OUT-OPERAC-RCTM
           MOVE WRK-INP-OPER-RCTM      TO WRK-OUT-OPER-RCTM
           MOVE WRK-INP-TECLPF9-RCTM   TO WRK-OUT-TECLPF9-RCTM
           MOVE WRK-INP-CARTE-RCTM     TO WRK-OUT-CARTE-RCTM
           MOVE WRK-INP-AGCONE-RCTM    TO WRK-OUT-AGCONE-RCTM

           MOVE WRK-INP-PAGINA-RCTM    TO WRK-OUT-PAGINA-RCTM
           MOVE WRK-INP-PAGINA1-RCTM   TO WRK-OUT-PAGINA1-RCTM
           MOVE WRK-INP-SITUAC-RCTM    TO WRK-OUT-SITUAC-RCTM
           MOVE WRK-INP-DESIDE-RCTM    TO WRK-OUT-DESIDE-RCTM
           MOVE WRK-INP-NUMERO-RCTM    TO WRK-OUT-NUMERO-RCTM
           MOVE WRK-INP-DTVENC-RCTM    TO WRK-OUT-DTVENC-RCTM
           MOVE WRK-INP-TPJURO-RCTM    TO WRK-OUT-TPJURO-RCTM
           MOVE WRK-INP-TPJUR-RCTM     TO WRK-OUT-TPJUR-RCTM
           MOVE WRK-INP-PARCEL-RCTM    TO WRK-OUT-PARCEL-RCTM
           MOVE WRK-INP-VALOR-RCTM     TO WRK-OUT-VALOR-RCTM
           MOVE WRK-INP-TAXA-RCTM      TO WRK-OUT-TAXA-RCTM
           MOVE WRK-INP-FREQTXOP-RCTM  TO WRK-OUT-FREQTXOP-RCTM.
           MOVE WRK-INP-TSMIN-RCTM     TO WRK-OUT-TSMIN-RCTM
           MOVE WRK-INP-TSMAX-RCTM     TO WRK-OUT-TSMAX-RCTM
           MOVE WRK-INP-PRODIA-RCTM    TO WRK-OUT-PRODIA-RCTM
           MOVE WRK-INP-PROMES-RCTM    TO WRK-OUT-PROMES-RCTM
           MOVE WRK-INP-PROANO-RCTM    TO WRK-OUT-PROANO-RCTM
           MOVE WRK-INP-TXJURO-RCTM    TO WRK-OUT-TXJURO-RCTM.
           MOVE WRK-INP-FREQTX-RCTM    TO WRK-OUT-FREQTX-RCTM.
           MOVE WRK-INP-MINMAX-RCTM    TO WRK-OUT-MINMAX-RCTM.
BRQ001     MOVE WRK-INP-LTBSPR-RCTM    TO WRK-OUT-LTBSPR-RCTM.

BRQ001     MOVE WRK-INP-CDBSPR-RCTM    TO WRK-OUT-CDBSPR-RCTM.
BRQ001     MOVE WRK-INP-DSBSPR-RCTM    TO WRK-OUT-DSBSPR-RCTM.
           MOVE WRK-INP-LITAUT-RCTM    TO WRK-OUT-LITAUT-RCTM.
           MOVE WRK-INP-CODAUT-RCTM    TO WRK-OUT-CODAUT-RCTM.
           MOVE WRK-INP-DESAUT-RCTM    TO WRK-OUT-DESAUT-RCTM.
           MOVE WRK-INP-JAUTOR-RCTM    TO WRK-OUT-JAUTOR-RCTM.
           MOVE WRK-INP-IAUTOR-RCTM    TO WRK-OUT-IAUTOR-RCTM.
           MOVE WRK-INP-TACAUT-RCTM    TO WRK-OUT-TACAUT-RCTM.
           MOVE WRK-INP-TOTAUT-RCTM    TO WRK-OUT-TOTAUT-RCTM.

           IF  WRK-INP-OPER-RCTM     NOT EQUAL SPACES AND LOW-VALUES
               INSPECT WRK-INP-OPER-RCTM REPLACING LEADING ' ' BY '0'
           END-IF.

           IF  WRK-INP-CARTE-RCTM     NOT EQUAL SPACES AND LOW-VALUES
               INSPECT WRK-INP-CARTE-RCTM REPLACING LEADING ' ' BY '0'
           END-IF.

           IF  WRK-INP-AGCONE-RCTM     NOT EQUAL SPACES AND LOW-VALUES
               INSPECT WRK-INP-AGCONE-RCTM REPLACING LEADING ' ' BY '0'
           END-IF.


           IF  WRK-INP-TXJURO-RCTM     NOT EQUAL SPACES AND LOW-VALUES
               INSPECT WRK-INP-TXJURO-RCTM REPLACING LEADING ' ' BY '0'
           END-IF.

           IF  WRK-INP-CODAUT-RCTM     NOT EQUAL SPACES AND LOW-VALUES
               INSPECT WRK-INP-CODAUT-RCTM REPLACING LEADING ' ' BY '0'
           END-IF.

           IF  WRK-INP-PRODIA-RCTM     NOT EQUAL SPACES AND LOW-VALUES
               INSPECT WRK-INP-PRODIA-RCTM REPLACING LEADING ' ' BY '0'
           END-IF.

           IF  WRK-INP-PROMES-RCTM     NOT EQUAL SPACES AND LOW-VALUES
               INSPECT WRK-INP-PROMES-RCTM REPLACING LEADING ' ' BY '0'
           END-IF.

           IF  WRK-INP-PROANO-RCTM     NOT EQUAL SPACES AND LOW-VALUES
               INSPECT WRK-INP-PROANO-RCTM REPLACING LEADING ' ' BY '0'
           END-IF.


BRQ001     IF  WRK-INP-CDBSPR-RCTM     NOT EQUAL SPACES AND LOW-VALUES
BRQ001         INSPECT WRK-INP-CDBSPR-RCTM REPLACING LEADING ' ' BY '0'
BRQ001         MOVE WRK-INP-CDBSPR-RCTM    TO WRK-OUT-CDBSPR-RCTM
BRQ001     END-IF.
BRQ001
      *----------------------------------------------------------------*
       2210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     RETORNAR AO PROGRAMA CHAMADOR - DCOM1802/DCOM1801          *
      *----------------------------------------------------------------*
       2220-RETORNAR-CHAMADOR          SECTION.
      *----------------------------------------------------------------*

           MOVE LENGTH OF WRK-COMU-AREA
                                          TO WRK-COMU-LL
           MOVE 'DCOM1803'                TO WRK-COMU-TRANSACAO
           MOVE WRK-INP-SENHAS-RCTM       TO WRK-COMU-SENHAS
           MOVE WRK-INP-COMANDO-RCTM      TO WRK-COMU-COMANDO
           MOVE WRK-INP-TMSTAMP-RCTM      TO WRK-COMU-TIMESTAMP

           MOVE WRK-INP-TIPOOPC-RCTM      TO WRK-COMU-OPCAO
           MOVE WRK-INP-TPSEL-RCTM        TO WRK-COMU-TP-SELECAO
           MOVE WRK-INP-PAGINA-RCTM       TO WRK-COMU-PAGINA-2
           MOVE WRK-INP-PAGINA1-RCTM      TO WRK-COMU-PAGINA-1
           MOVE WRK-INP-OPERAC-RCTM(1:4)  TO WRK-COMU-DANO-OPER
           MOVE WRK-INP-OPERAC-RCTM(5:9)  TO WRK-COMU-NSEQ-OPER
           MOVE 237                       TO WRK-COMU-BANCO
           MOVE WRK-INP-XCONTA-RCTM(5:5)  TO WRK-COMU-AGENCIA
           MOVE WRK-INP-XCONTA-RCTM(11:13)   TO WRK-COMU-CONTA
BSI001     MOVE WRK-INP-CNPJ1-RCTM        TO WRK-CNPJ-1.
BSI001     MOVE WRK-INP-CNPJ2-RCTM        TO WRK-CNPJ-2.
BSI001     MOVE WRK-INP-CNPJ3-RCTM        TO WRK-CNPJ-3.
BSI001     MOVE WRK-NRO-CNPJ              TO WRK-COMU-CCNPJ-CPF.
BSI001     MOVE WRK-INP-CNPJ4-RCTM        TO WRK-COMU-CFLIAL-CNPJ.
BSI001     MOVE WRK-INP-CNPJ5-RCTM        TO WRK-COMU-CCTRL-CNPJCPF.
           MOVE WRK-INP-CHAMA-RCTM        TO WRK-COMU-CHAMADOR
           MOVE WRK-INP-CHAMADO-RCTM     TO WRK-TELA
           MOVE WRK-CHNG                 TO WRK-FUNCAO
           MOVE WRK-COMU-AREA            TO WRK-MENSAGEM.

      *----------------------------------------------------------------*

       2220-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     RETORNAR AO MENU ROTINAS                                   *
      *----------------------------------------------------------------*
       2230-MENU-ROTINAS               SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                     WRK-COMU-AREA
           MOVE LENGTH                 OF WRK-COMU-AREA
                                       TO WRK-COMU-LL.
           MOVE WRK-CHNG               TO WRK-FUNCAO
           MOVE 'DCOM1803'             TO WRK-TELA
                                          WRK-COMU-TRANSACAO.
           MOVE WRK-INP-SENHAS-RCTM    TO WRK-COMU-SENHAS
           MOVE WRK-INP-COMANDO-RCTM   TO WRK-COMU-COMANDO
           MOVE '5'                    TO WRK-COMU-PFK
           MOVE WRK-COMU-AREA          TO WRK-MENSAGEM.

      *----------------------------------------------------------------*

       2230-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     CONFIRMAR VALORES DA TELA                                  *
      *----------------------------------------------------------------*
       2250-CONFIRMA                   SECTION.
      *----------------------------------------------------------------*

BSI001     MOVE WRK-COD-DEPTO-N        TO WRK-5470E-COD-DEPTO.
BSI001     MOVE WRK-COD-USER-R         TO WRK-5470E-CFUNC-BDSCO
           MOVE LNK-IO-LTERM           TO WRK-5470E-CTERM
           MOVE WRK-INP-CODPRD-RCTM    TO WRK-5470E-PRODUTO
           MOVE WRK-INP-CODSUB-RCTM    TO WRK-5470E-SUBPRODUTO
           MOVE WRK-INP-XCONTA-RCTM(1:3)
                                       TO WRK-5470E-BANCO
           MOVE WRK-INP-XCONTA-RCTM(5:5)
                                       TO WRK-5470E-AGENCIA
           MOVE WRK-INP-XCONTA-RCTM(11:13)
                                      TO WRK-5470E-CONTA
BSI001     MOVE WRK-INP-CNPJ1-RCTM     TO WRK-CNPJ-1.

BSI001     MOVE WRK-INP-CNPJ2-RCTM     TO WRK-CNPJ-2.
BSI001     MOVE WRK-INP-CNPJ3-RCTM     TO WRK-CNPJ-3.
BSI001     MOVE WRK-NRO-CNPJ           TO WRK-5470E-CCNPJ-CPF-CEDENTE
BSI001     MOVE WRK-INP-CNPJ4-RCTM     TO WRK-5470E-CFLIAL-CNPJ-CEDENTE
BSI001     MOVE WRK-INP-CNPJ5-RCTM     TO WRK-5470E-CCTRL-CNPJ-CEDENTE

           MOVE WRK-INP-OPERAC-RCTM   TO WRK-5470E-OPERACAO
           IF   WRK-INP-AGCONE-RCTM        EQUAL SPACES
                MOVE ZEROS                 TO WRK-5470E-CAGE-CONS-CONE
           ELSE
                MOVE WRK-INP-AGCONE-RCTM   TO WRK-5470E-CAGE-CONS-CONE
           END-IF
           MOVE WRK-INP-CODDESC-RCTM  TO WRK-5470E-TP-DESCONTO
           MOVE WRK-INP-PARCEL-RCTM   TO WRK-5470E-NPCELA-DESC-COML
           MOVE 'N'                   TO WRK-5470E-MOTVO-ADIT

           IF   WRK-INP-CARTE-RCTM  EQUAL SPACES
                MOVE SPACES              TO WRK-5470E-CCAR-CONS-CONE
           ELSE
                MOVE WRK-INP-CARTE-RCTM  TO WRK-5470E-CCAR-CONS-CONE
           END-IF

           IF   WRK-INP-OPER-RCTM   EQUAL SPACES
                MOVE ZEROS             TO WRK-5470E-DANO-CONS-CONE
                                          WRK-5470E-NSEQ-CONS-CONE
           ELSE
                MOVE WRK-INP-OPER-RCTM(1:4) TO WRK-5470E-DANO-CONS-CONE
                MOVE WRK-INP-OPER-RCTM(5:9) TO WRK-5470E-NSEQ-CONS-CONE
           END-IF
           MOVE WRK-INP-TAXA-RCTM      TO WRK-5470E-PTX-JURO-MES-DE
           MOVE WRK-INP-FREQTX-RCTM    TO WRK-5470E-TFREQ-TAXA-PCELA-DE
           MOVE WRK-INP-VALOR-RCTM     TO WRK-5470E-VLR-PARCELA
           MOVE WRK-INP-PRODIA-RCTM-N  TO WRK-DT-AUX-DD
           MOVE WRK-INP-PROMES-RCTM-N  TO WRK-DT-AUX-MM
           MOVE WRK-INP-PROANO-RCTM-N  TO WRK-DT-AUX-AA

           INSPECT WRK-DT-AUX
           REPLACING ALL '/' BY '.'

           MOVE WRK-DT-AUX            TO WRK-5470E-DVCTO-PCELA-PARA

           INITIALIZE  WRK-AREA-BRAD0496


           MOVE WRK-INP-TXJURO-RCTM    TO WRK-VALOR-0496.
           MOVE 09080305               TO WRK-TAMANHO-0496.
           MOVE SPACES                 TO WRK-SAIDA-0496.

           CALL 'BRAD0496'             USING WRK-VALOR-0496
                                             WRK-SAIDA-0496
                                             WRK-TAMANHO-0496.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-INP-TXJURO-RCTM-N
                                        TO WRK-5470E-PTX-JURO-MES-PARA
           ELSE
               MOVE WRK-SAIDA-0496-N    TO WRK-5470E-PTX-JURO-MES-PARA
           END-IF

           INITIALIZE  WRK-AREA-BRAD0496-4


           MOVE WRK-INP-JAUTOR-RCTM-N  TO WRK-VALOR-0496-4.
           MOVE 14110902               TO WRK-TAMANHO-0496-4.
           MOVE SPACES                 TO WRK-SAIDA-0496-4.



           CALL 'BRAD0496'             USING WRK-VALOR-0496-4
                                             WRK-SAIDA-0496-4
                                             WRK-TAMANHO-0496-4.


           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE  WRK-INP-JAUTOR-RCTM-N TO WRK-5470E-VJURO-PRROG-PARA
           ELSE
               MOVE WRK-SAIDA-0496-N-4     TO WRK-5470E-VJURO-PRROG-PARA
           END-IF


           MOVE WRK-INP-VLRIOF-RCTM   TO WRK-5470E-PIOF-PRROG-PARA

           IF   WRK-INP-IAUTOR-RCTM    NOT EQUAL SPACES
                INITIALIZE  WRK-AREA-BRAD0496-4
                MOVE WRK-INP-IAUTOR-RCTM    TO WRK-VALOR-0496-4
                MOVE 14110902               TO WRK-TAMANHO-0496-4
                MOVE SPACES                 TO WRK-SAIDA-0496-4


                CALL 'BRAD0496'             USING WRK-VALOR-0496-4
                                             WRK-SAIDA-0496-4
                                             WRK-TAMANHO-0496-4

                IF  RETURN-CODE             NOT EQUAL ZEROS
                    MOVE  ZEROS       TO WRK-5470E-VIOF-PRROG-PARA
                ELSE
                    MOVE WRK-SAIDA-0496-N-4
                                       TO WRK-5470E-VIOF-PRROG-PARA
                END-IF
           ELSE
                MOVE ZEROS             TO WRK-5470E-VIOF-PRROG-PARA
           END-IF


           INITIALIZE  WRK-AREA-BRAD0496-4

           IF   WRK-INP-TACAUT-RCTM    NOT EQUAL SPACES
                MOVE WRK-INP-TACAUT-RCTM    TO WRK-VALOR-0496-4
                MOVE 14110902               TO WRK-TAMANHO-0496-4

                MOVE SPACES                 TO WRK-SAIDA-0496-4

                CALL 'BRAD0496'             USING WRK-VALOR-0496-4
                                             WRK-SAIDA-0496-4
                                             WRK-TAMANHO-0496-4


                IF  RETURN-CODE             NOT EQUAL ZEROS AND 20
                    MOVE  WRK-INP-TACAUT-RCTM
                                      TO WRK-5470E-VTAXA-PRROG-PARA
                ELSE
                    MOVE WRK-SAIDA-0496-N-4
                                      TO WRK-5470E-VTAXA-PRROG-PARA
                END-IF
           ELSE
                MOVE ZEROS              TO WRK-5470E-VTAXA-PRROG-PARA
           END-IF

           MOVE WRK-INP-FREQTX-RCTM   TO WRK-5470E-TFREQ-TAXA-PARA

           IF   WRK-INP-TXMIN-RCTM     NOT EQUAL SPACES

                MOVE WRK-INP-TXMIN-RCTM     TO WRK-VALOR-0496-2
                MOVE 06050203               TO WRK-TAMANHO-0496-2
                MOVE SPACES                 TO WRK-SAIDA-0496-2

                CALL 'BRAD0496'             USING WRK-VALOR-0496-2
                                             WRK-SAIDA-0496-2
                                             WRK-TAMANHO-0496-2

               IF  RETURN-CODE             NOT EQUAL ZEROS
                   MOVE ZEROS              TO WRK-TAXA-JURO-MIN
               ELSE
                   MOVE WRK-SAIDA-0496-2-N TO
                                           WRK-5470E-PMIN-TAXA-JURO-PARA
               END-IF
           ELSE
               MOVE ZEROS               TO WRK-5470E-PMIN-TAXA-JURO-PARA
           END-IF

           IF   WRK-INP-TXMAX-RCTM     NOT EQUAL SPACES
                MOVE WRK-INP-TXMAX-RCTM     TO WRK-VALOR-0496-2
                MOVE 06050203               TO WRK-TAMANHO-0496-2

                MOVE SPACES                 TO WRK-SAIDA-0496-2

                CALL 'BRAD0496'             USING WRK-VALOR-0496-2
                                             WRK-SAIDA-0496-2
                                             WRK-TAMANHO-0496-2

                IF  RETURN-CODE             NOT EQUAL ZEROS
                   MOVE ZEROS              TO WRK-TAXA-JURO-MAX
                ELSE
                   MOVE WRK-SAIDA-0496-2-N
                                       TO WRK-5470E-PMAX-TAXA-JURO-PARA
                END-IF
           ELSE
                MOVE ZEROS             TO WRK-5470E-PMAX-TAXA-JURO-PARA
           END-IF

           MOVE WRK-INP-TPJUR-RCTM    TO WRK-5470E-CTPO-TX-JURO
           MOVE WRK-INP-TPJURO-RCTM   TO WRK-5470E-ITPO-TX-JURO

           IF  WRK-INP-CODAUT-RCTM-N GREATER ZEROS
               MOVE WRK-INP-CODAUT-RCTM-N TO WRK-5470E-COD-AUTORIZANTE

           ELSE
               MOVE ZEROS                 TO WRK-5470E-COD-AUTORIZANTE
           END-IF

           IF  WRK-INP-DESAUT-RCTM   NOT EQUAL SPACES
               MOVE WRK-INP-DESAUT-RCTM   TO WRK-5470E-DESC-AUTORIZANTE
           ELSE
               MOVE SPACES                TO WRK-5470E-DESC-AUTORIZANTE
           END-IF

BRQ001     IF  WRK-INP-LTBSPR-RCTM     NOT EQUAL SPACES
BRQ001         MOVE WRK-INP-CDBSPR-RCTM-N  TO WRK-5470E-CDBASE-RURAL
BRQ001     END-IF.
BRQ001
           PERFORM 2251-INCLUSAO
           .

      *----------------------------------------------------------------*
       2250-99-FIM.                    EXIT.
      *----------------------------------------------------------------*


      *----------------------------------------------------------------*
      *     INCLUSAO VIA O MODULO DCOM5345                             *
      *----------------------------------------------------------------*
       2251-INCLUSAO                   SECTION.
      *----------------------------------------------------------------*

           MOVE 'DCOM5470'             TO WRK-MODULO

           CALL WRK-MODULO             USING WRK-5470-ENTRADA
                                             WRK-5470-SAIDA
                                             ERRO-AREA
                                             WRK-SQLCA.

           EVALUATE WRK-5470S-COD-RETORNO
             WHEN '0000'
                   MOVE WRK-241        TO  WRK-OUT-PRODIA-ATTR-RCTM
                                           WRK-OUT-PROMES-ATTR-RCTM
                                           WRK-OUT-PROANO-ATTR-RCTM
                                           WRK-OUT-TXJURO-ATTR-RCTM
                                           WRK-OUT-TPJURO-ATTR-RCTM
                                           WRK-OUT-CODAUT-ATTR-RCTM

                                           WRK-OUT-DESAUT-ATTR-RCTM

                   MOVE 'INCLUSAO EFETUADA COM SUCESSO'
                                           TO WRK-COMU-MENSAGEM

                   IF WRK-FLAG-CONSIST   EQUAL SPACES
                      MOVE '6'           TO WRK-COMU-PFK
                      PERFORM  2220-RETORNAR-CHAMADOR
                   END-IF

             WHEN '0099'
                   MOVE 'DB2'         TO ERR-TIPO-ACESSO
                   PERFORM 9999-PROCESSAR-ROTINA-ERRO

             WHEN '0501'
             WHEN '1372'

                   IF  WRK-5470E-FLAG-CONSIST EQUAL 'C'
                       MOVE '3'       TO WRK-OUT-PASSO-RCTM
                       MOVE WRK-49361 TO WRK-OUT-COMANDO-ATTR-RCTM
                       MOVE 'TECLE <PF9> PARA CONSULTAR CREDITO OU <ENTE

      -                     'R> PARA ALTERAR'
                                      TO WRK-OUT-MENSA-RCTM
                       MOVE WRK-PNNS  TO  WRK-OUT-TXJURO-ATTR-RCTM
                       MOVE WRK-PNNS  TO  WRK-OUT-PRODIA-ATTR-RCTM
                       MOVE WRK-PNNS  TO  WRK-OUT-PROMES-ATTR-RCTM
                       MOVE WRK-PNNS  TO  WRK-OUT-PROANO-ATTR-RCTM
                   ELSE
                       MOVE '1'       TO WRK-OUT-PASSO-RCTM
                       MOVE WRK-5470S-MSG-RETORNO
                                      TO WRK-OUT-MENSA-RCTM
BRQ001                 IF  WRK-INP-LTBSPR-RCTM NOT EQUAL SPACES
BRQ001                     MOVE WRK-209  TO WRK-OUT-CDBSPR-ATTR-RCTM
BRQ001                 END-IF
                   END-IF
                   MOVE '@'           TO WRK-INCONSIS

             WHEN OTHER
                   MOVE WRK-5470S-MSG-RETORNO
                                      TO WRK-OUT-MENSA-RCTM
                   MOVE '*'           TO WRK-INCONSIS


           END-EVALUATE.

      *----------------------------------------------------------------*
       2251-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA EFETUA A CONSULTA AO PROGRAMA DCOM0105            *
      *----------------------------------------------------------------*
       2260-TRATA-CONSULTA      SECTION.
      *----------------------------------------------------------------*

           PERFORM 1263-GRAVAR-DBTEMP.

           PERFORM 1261-ACESSAR-DCOM4174

           INITIALIZE COMU-AREA-0105

           MOVE  LENGTH OF COMU-AREA-0105
                                        TO  COMU-LL-0105
           MOVE  ZEROS                  TO  COMU-ZZ-0105.

           MOVE  'S'                    TO  WRK-OUT-TECLPF9-RCTM.
           MOVE  WRK-CHNG               TO  WRK-FUNCAO.
           MOVE  'DCOM0105'             TO  WRK-TELA.
           MOVE  'DCOM1803'             TO  COMU-TRANSACAO-0105.
           MOVE  WRK-OUT-SENHAS-RCTM    TO  COMU-SENHAS-0105
           MOVE  '1'                    TO  COMU-TPOPCAO-0105
           MOVE  WRK-OUT-TMSTAMP-RCTM   TO  COMU-TMSTAMP-0105
           MOVE  WRK-OUT-NOMECL-RCTM    TO  COMU-NOME-0105
           MOVE  WRK-OUT-XCONTA-RCTM    TO  COMU-BCOAGECTA-0105
           MOVE  WRK-OUT-CODPRD-RCTM    TO  COMU-CODPROD-0105
           MOVE  WRK-OUT-DESPRD-RCTM    TO  COMU-DESPROD-0105
           MOVE  WRK-OUT-CODSUB-RCTM    TO  COMU-CODSUBP-0105
           MOVE  WRK-OUT-DESSUB-RCTM    TO  COMU-DESSUBP-0105
           MOVE  4174-SAI-CMEIO-ENTRD-DESC
                                        TO  COMU-MEIOENT-0105
           MOVE  WRK-OUT-OPERAC-RCTM  TO  COMU-OPERACAO-0105
           MOVE  WRK-OUT-SITUAC-RCTM    TO  COMU-SITUAC-0105
           MOVE  4174-SAI-CELMTO-DESC-COML
                                        TO COMU-TPDESCONV-0105
           IF  4174-SAI-NSMULA-OPER-DESC  GREATER ZEROS
               MOVE  4174-SAI-NSMULA-OPER-DESC

                                       TO  COMU-NSIMULA-0105
               MOVE 'SIMULACAO...:'    TO  COMU-DSIMULA-0105
           END-IF

           IF  CAMPO4      IS NUMERIC  AND
               CAMPO4         GREATER  ZEROS
               MOVE  CAMPO4            TO  COMU-NUMERO-0105-N
           END-IF

           MOVE  CAMPO3                      TO  COMU-DESCIDEN-0105

           MOVE  4174-SAI-CCART-LIM-CREDT    TO  COMU-CCART-0105
           MOVE  4174-SAI-DANO-BASE          TO  COMU-NRCOUM-0105

           IF  4174-SAI-CTPO-DESC-COML    EQUAL 7 OR 8
               PERFORM 231000-OBTER-DADOS-CLIENTE
           END-IF

           MOVE  WRK-OUT-CGCCPF-RCTM(1:3)   TO  COMU-CCNPJ-0105(1:3)
           MOVE  WRK-OUT-CGCCPF-RCTM(5:3)   TO  COMU-CCNPJ-0105(4:3)
           MOVE  WRK-OUT-CGCCPF-RCTM(9:3)   TO  COMU-CCNPJ-0105(7:3)

           MOVE  WRK-OUT-CGCCPF-RCTM(13:4)  TO  COMU-CFLIAL-0105
           MOVE  WRK-OUT-CGCCPF-RCTM(18:2)  TO  COMU-CTRL-0105

           MOVE  4174-SAI-CDEPDC             TO  COMU-NRCODOIS-0105
           MOVE  4174-SAI-CSEQ-STUDO         TO  COMU-NRCOTRES-0105
           MOVE  4174-SAI-CTPO-DESC-COML     TO  COMU-TIPDES-0105


           IF  4174-SAI-CELMTO-DESC-COML EQUAL 09
               PERFORM 1262-ACESSAR-DCOM5259
               MOVE 009                TO COMU-TIPDES-0105
           END-IF.

           MOVE  COMU-AREA-0105              TO  WRK-MENSAGEM.

      *----------------------------------------------------------------*
       2260-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1261-ACESSAR-DCOM4174           SECTION.

      *----------------------------------------------------------------*

           MOVE SPACES                 TO 4174-ENT-COD-RETORNO
                                          4174-ENT-MSG-RETORNO
           MOVE ZEROS                  TO 4174-ENT-RESTART
           MOVE 'N'                    TO 4174-ENT-FLAG
           MOVE WRK-COD-USER           TO 4174-ENT-CFUNC-BDSCO
           MOVE LNK-IO-LTERM           TO 4174-ENT-CTERM
           MOVE WRK-INP-OPERAC-RCTM(1:4)  TO 4174-ENT-DANO-OPER-DESC
           MOVE WRK-INP-OPERAC-RCTM(5:9)  TO 4174-ENT-NSEQ-OPER-DESC
           MOVE 'DCOM4174'             TO WRK-MODULO.


           CALL WRK-MODULO             USING 4174-ENTRADA-ROTEADOR
                                             4174-SAIDA-ROTEADOR
                                             ERRO-AREA
                                             WRK-SQLCA.

           EVALUATE 4174-SAI-COD-RETORNO

               WHEN '0000'


                  IF  4174-SAI-CCONTR-CONVE-DESC NOT EQUAL ZEROS
                      IF  4174-SAI-CELMTO-DESC-COML  EQUAL 8
                          MOVE 'CONV GERAL..:'       TO CAMPO3
                      ELSE
                          MOVE 'CONV CLIENTE:'       TO CAMPO3
                      END-IF
                      MOVE 4174-SAI-CCONTR-CONVE-DESC
                                                 TO CAMPO4
                  ELSE
                      IF  4174-SAI-CCONTR-LIM-DESC NOT EQUAL ZEROS
                          MOVE 'CONTR LIMITE:'   TO CAMPO3
                          MOVE 4174-SAI-CCONTR-LIM-DESC
                                                 TO CAMPO4
                      END-IF
                  END-IF

               WHEN '0099'
                    MOVE  'DB2'                TO ERR-TIPO-ACESSO
                    PERFORM 9999-PROCESSAR-ROTINA-ERRO


               WHEN OTHER
                    MOVE '*'                   TO WRK-INCONSIS
                    MOVE 4174-SAI-MSG-RETORNO  TO  WRK-OUT-MENSA-RCTM

           END-EVALUATE.

      *----------------------------------------------------------------*
       1261-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1262-ACESSAR-DCOM5259           SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WRK-5259-ENTRADA
BSI001                WRK-5259-SAIDA.

           MOVE WRK-COD-USER-R         TO WRK-5259E-CFUNC-BDSCO
           MOVE LNK-IO-LTERM           TO WRK-5259E-CTERM
BSI001     MOVE WRK-INP-NUMERO-RCTM-N  TO WRK-5259E-CCONVE-CLI-DESC
           MOVE 4174-SAI-CVRSAO-CONTR-CONVE

                                       TO WRK-5259E-CVRSAO-CONVE-CLI
           MOVE 'DCOM5259'             TO WRK-MODULO.

           CALL  WRK-MODULO            USING WRK-5259-ENTRADA
                                             WRK-5259-SAIDA
                                             ERRO-AREA
                                             WRK-SQLCA.

           IF  WRK-5259S-COD-RETORNO   NOT EQUAL ZEROS
               IF  WRK-5259S-COD-RETORNO EQUAL '0099'
                   MOVE 'DB2'          TO ERR-TIPO-ACESSO
               ELSE
                   MOVE 'APL'          TO ERR-TIPO-ACESSO
               END-IF
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           ELSE
               MOVE WRK-5259S-CCNPJ-CPF   TO COMU-CCNPJ-CON-0105
               MOVE WRK-5259S-CFLIAL-CNPJ TO COMU-CFLIAL-CON-0105
               MOVE WRK-5259S-CCTRL-CNPJ  TO COMU-CTRL-CON-0105
               MOVE WRK-5259S-CPRODT      TO COMU-PRODCONV-0105
               MOVE WRK-5259S-CSPROD-DESC-COML

                                          TO COMU-SPRODCONV-0105
           END-IF.

      *----------------------------------------------------------------*
       1262-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1263-GRAVAR-DBTEMP              SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WRK-ENTRADA-DCOM7995
                      WRK-SAIDA-DCOM7995.

           MOVE 'I '                   TO WRK-ENT-OPCAO
           MOVE WRK-COD-USER-R         TO WRK-ENT-COD-FUNC
           MOVE LNK-IO-LTERM           TO WRK-ENT-COD-TERMINAL
           MOVE WRK-INP-TMSTAMP-RCTM   TO WRK-ENT-TIMESTAMP
           MOVE 'DCOM1803'             TO WRK-ENT-TRANS
           MOVE SPACES                 TO WRK-ENT-TRANS-ANT
           MOVE 1                      TO WRK-ENT-PAGINA

           MOVE WRK-INPUT-RCTM         TO WRK-ENT-DADOS.

           MOVE 'DCOM7995'             TO WRK-MODULO

           CALL WRK-MODULO             USING WRK-ENTRADA-DCOM7995
                                             WRK-SAIDA-DCOM7995
                                             ERRO-AREA
                                             WRK-SQLCA.

           IF  WRK-SAI-COD-RETORNO     EQUAL 01
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE WRK-SAI-MSG-RETORNO
                                       TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           ELSE
               IF  WRK-SAI-COD-RETORNO EQUAL 99
                   MOVE 'DB2'          TO ERR-TIPO-ACESSO
                   PERFORM 9999-PROCESSAR-ROTINA-ERRO
               END-IF
           END-IF.


      *----------------------------------------------------------------*
       1263-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       231000-OBTER-DADOS-CLIENTE      SECTION.
      *----------------------------------------------------------------*

           INITIALIZE 4170-ENTRADA

           MOVE  4174-ENT-DANO-OPER-DESC TO  WRK-DANO-OPER-DESC-1
           MOVE  4174-ENT-NSEQ-OPER-DESC TO  WRK-NSEQ-OPER-DESC-1
           MOVE  SPACES                  TO  4170-ENT-CCHAVE-ELMTO-DESC
           MOVE  WRK-CHAVE               TO  4170-ENT-CCHAVE-ELMTO-DESC
           MOVE  'SE'                    TO  4170-ENT-CINDCD-PSSOA-DESC
           MOVE  6                       TO  4170-ENT-CELMTO-DESC-COML

           MOVE  'DCOM4170'            TO  WRK-MODULO
           CALL  WRK-MODULO            USING 4170-ENTRADA
                                             4170-SAIDA
                                             ERRO-AREA

                                             WRK-SQLCA

           IF  4170-SAI-COD-RET        NOT EQUAL ZEROS
               MOVE  4170-SAI-MSG-RET     TO WRK-OUT-MENSA-RCTM
               GO TO 231000-99-FIM
           END-IF

           MOVE 4170-SAI-CCNPJ-CPF         TO COMU-CCNPJ-EMI-0105
           MOVE 4170-SAI-CFLIAL-CNPJ(2:4)  TO COMU-CFLIAL-EMI-0105
           MOVE 4170-SAI-CCTRL-CNPJ-CPF    TO COMU-CTRL-EMI-0105.

      *----------------------------------------------------------------*
       231000-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     TRATAR ATRIBUTOS PARA DCOM4473                             *
      *----------------------------------------------------------------*
       2262-TRATAR-ATRIB-4473          SECTION.
      *----------------------------------------------------------------*


           IF  WRK-OUT-FLAG-RCTM    EQUAL 'S'   AND
               WRK-OUT-TPJUR-RCTM   NOT EQUAL 3
               MOVE WRK-225         TO WRK-OUT-MINMAX-ATTR-RCTM
           ELSE
               MOVE WRK-229         TO WRK-OUT-MINMAX-ATTR-RCTM
           END-IF.

           IF  WRK-OUT-TPJUR-RCTM   EQUAL 3
               MOVE WRK-225         TO WRK-OUT-TXJURO-ATTR-RCTM
           END-IF.

      *----------------------------------------------------------------*
       2262-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     RETORNAR AO MENU DCOM                                      *
      *----------------------------------------------------------------*
       2270-MENU-DCOM                  SECTION.
      *----------------------------------------------------------------*


           INITIALIZE                     WRK-COMU-AREA
           MOVE LENGTH OF WRK-COMU-AREA
                                       TO WRK-COMU-LL
           MOVE WRK-CHNG               TO WRK-FUNCAO
           MOVE 'DCOM1803'             TO WRK-TELA
                                          WRK-COMU-TRANSACAO
           MOVE WRK-OUT-SENHAS-RCTM    TO WRK-COMU-SENHAS
           MOVE WRK-OUT-COMANDO-RCTM   TO WRK-COMU-COMANDO
           MOVE 'A'                    TO WRK-COMU-PFK
           MOVE WRK-COMU-AREA          TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       2270-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2280-TRATAR-ENTER               SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO  WRK-INCONSIS


BRQ001     IF  WRK-INP-LTBSPR-RCTM     NOT EQUAL SPACES
BRQ001         MOVE WRK-209            TO WRK-OUT-CDBSPR-ATTR-RCTM
BRQ001     END-IF.

           PERFORM 2280-50-ACESSAR-DCOM5335.

           IF  WRK-INCONSIS                EQUAL  '*'
               GO TO 2280-99-FIM
           END-IF.

           PERFORM 2281-CONSIST-FISICA

           IF  WRK-INCONSIS                EQUAL  '*'
               GO TO 2280-99-FIM
           END-IF.

BRQ001     IF  WRK-INP-LTBSPR-RCTM     NOT EQUAL SPACES
BRQ001         PERFORM 2288-CONSISTIR-BASE-PRORROG
BRQ001         IF  WRK-INCONSIS        EQUAL  '*'
BRQ001             GO TO 2280-99-FIM
BRQ001         END-IF

BRQ001     END-IF.

           PERFORM 2286-CONSISTIR-TAXA

           IF  WRK-INCONSIS                EQUAL  '*'
               GO TO 2280-99-FIM
           END-IF.

           PERFORM 2285-ACESSAR-DCOM5424

           PERFORM 2262-TRATAR-ATRIB-4473

           IF  WRK-INCONSIS                EQUAL  '*'
               GO TO 2280-99-FIM
           END-IF.

           PERFORM 2283-CONSISTIR-TXJURO-MENSAL

           IF  WRK-INCONSIS                       EQUAL SPACES

               PERFORM 2284-CONSISTIR-INTERVALOR-JURO


               IF  WRK-INCONSIS                   EQUAL SPACES
                   EVALUATE TRUE
                       WHEN (WRK-INP-CODAUT-RCTM  NOT NUMERIC  OR
                            WRK-INP-CODAUT-RCTM-N EQUAL ZEROS) AND
                            WRK-OUT-LITAUT-RCTM   NOT EQUAL SPACES
                            MOVE '*'              TO WRK-INCONSIS
                            MOVE WRK-49369  TO WRK-OUT-CODAUT-ATTR-RCTM
                            MOVE SG-MENSAGEM(138) TO WRK-OUT-MENSA-RCTM
                            GO TO 2280-99-FIM

                   END-EVALUATE
               ELSE
                   GO TO 2280-99-FIM
               END-IF
           END-IF.

           PERFORM 2282-OBTER-TOTAIS

           IF  WRK-INCONSIS   EQUAL SPACES
               INITIALIZE           WRK-5470-ENTRADA

                                    WRK-5470-SAIDA
               MOVE 'C'             TO WRK-5470E-FLAG-CONSIST
                                       WRK-FLAG-CONSIST
               PERFORM 2250-CONFIRMA
           END-IF

           IF  WRK-INCONSIS   EQUAL '@'
               MOVE WRK-OUT-TXJURO-RCTM-N  TO WRK-TAXA-1
               MOVE WRK-OUT-TAXA-RCTM      TO WRK-TAXA-2
               IF WRK-TAXA-1               EQUAL WRK-TAXA-2
                  MOVE WRK-229             TO WRK-OUT-MINMAX-ATTR-RCTM
               END-IF
               MOVE WRK-49361 TO WRK-OUT-COMANDO-ATTR-RCTM
               GO TO 2280-99-FIM
           END-IF.

           IF  WRK-INCONSIS   EQUAL SPACES
               MOVE WRK-49361              TO WRK-OUT-COMANDO-ATTR-RCTM
               MOVE 'TECLE <PF6> PARA CONFIRMAR OU <ENTER> PARA ALTERAR'
                                           TO  WRK-OUT-MENSA-RCTM
               MOVE WRK-PNNS               TO  WRK-OUT-TXJURO-ATTR-RCTM

               MOVE WRK-PNNS               TO  WRK-OUT-PRODIA-ATTR-RCTM
               MOVE WRK-PNNS               TO  WRK-OUT-PROMES-ATTR-RCTM
               MOVE WRK-PNNS               TO  WRK-OUT-PROANO-ATTR-RCTM
BRQ001         MOVE WRK-241                TO  WRK-OUT-CDBSPR-ATTR-RCTM
               MOVE '2'                    TO  WRK-OUT-PASSO-RCTM
           ELSE
               MOVE SPACES                 TO  WRK-OUT-JAUTOR-RCTM
                                               WRK-OUT-IAUTOR-RCTM
                                               WRK-OUT-TACAUT-RCTM
                                               WRK-OUT-TOTAUT-RCTM

BSI001         IF  WRK-5470S-COD-RETORNO   EQUAL '1371'
BSI001             MOVE WRK-49369          TO WRK-OUT-TXJURO-ATTR-RCTM
BSI001         ELSE
                   MOVE WRK-49369          TO  WRK-OUT-PRODIA-ATTR-RCTM
                                               WRK-OUT-PROMES-ATTR-RCTM
                                               WRK-OUT-PROANO-ATTR-RCTM
BSI001         END-IF

BRQ001         IF  WRK-5470S-COD-RETORNO   EQUAL '1406' OR '1407'
BRQ001             MOVE WRK-49369          TO WRK-OUT-CDBSPR-ATTR-RCTM

BRQ001             MOVE SPACES             TO WRK-OUT-DSBSPR-RCTM
BRQ001         ELSE
BRQ001             IF  WRK-INP-LTBSPR-RCTM NOT EQUAL SPACES
BRQ001                 MOVE WRK-209        TO WRK-OUT-CDBSPR-ATTR-RCTM
BRQ001             END-IF
BRQ001         END-IF
BRQ001
PRIME          IF  WRK-5470S-COD-RETORNO   EQUAL '0400'
PRIME              MOVE WRK-49369          TO WRK-OUT-CODAUT-ATTR-RCTM
PRIME              MOVE WRK-209            TO WRK-OUT-PRODIA-ATTR-RCTM
PRIME                                         WRK-OUT-PROMES-ATTR-RCTM
PRIME                                         WRK-OUT-PROANO-ATTR-RCTM
PRIME                                         WRK-OUT-TXJURO-ATTR-RCTM
PRIME              IF  WRK-OUT-FLAG-RCTM   EQUAL 'S'   AND
PRIME                  WRK-OUT-TPJUR-RCTM  NOT EQUAL 3
PRIME                  MOVE WRK-225        TO WRK-OUT-MINMAX-ATTR-RCTM
PRIME              ELSE
PRIME                  MOVE WRK-229        TO WRK-OUT-MINMAX-ATTR-RCTM
PRIME              END-IF
PRIME          END-IF
           END-IF.


           MOVE WRK-OUT-TXJURO-RCTM-N  TO WRK-TAXA-1.
           MOVE WRK-OUT-TAXA-RCTM      TO WRK-TAXA-2.

           IF WRK-TAXA-1            EQUAL WRK-TAXA-2
              MOVE WRK-229          TO WRK-OUT-MINMAX-ATTR-RCTM
           END-IF.

      *----------------------------------------------------------------*
       2280-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2280-50-ACESSAR-DCOM5335        SECTION.
      *----------------------------------------------------------------*

           INITIALIZE 5335-ENTRADA
                      5335-SAIDA.

           MOVE WRK-INP-OPERAC-RCTM    TO 5335E-OPERACAO.
           MOVE WRK-INP-PARCEL-RCTM    TO 5335E-NPCELA-DESC-COML.

           MOVE 007                    TO 5335E-CELMTO-DESC-COML.
           MOVE 008                    TO 5335E-CEVNTO-DESC-COML
           MOVE 'I'                    TO 5335E-ACAO.
           MOVE 'DCOM5335'             TO WRK-MODULO.

           CALL WRK-MODULO             USING 5335-ENTRADA
                                             5335-SAIDA
                                             ERRO-AREA
                                             WRK-SQLCA

           EVALUATE  5335S-COD-RETORNO
               WHEN '0000'
                     CONTINUE

               WHEN '0099'
                     MOVE 'DB2'        TO ERR-TIPO-ACESSO
                     PERFORM 9999-PROCESSAR-ROTINA-ERRO

               WHEN  OTHER
                     MOVE 5335S-MSG-RETORNO
                                       TO WRK-OUT-MENSA-RCTM

                     MOVE '*'          TO WRK-INCONSIS
           END-EVALUATE.

      *----------------------------------------------------------------*
       2280-50-99-FIM.                 EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2281-CONSIST-FISICA             SECTION.
      *----------------------------------------------------------------*

           IF  WRK-INP-PRODIA-RCTM     EQUAL SPACES OR LOW-VALUES OR
               WRK-INP-PROMES-RCTM     EQUAL SPACES OR LOW-VALUES OR
               WRK-INP-PROANO-RCTM     EQUAL SPACES OR LOW-VALUES
               MOVE WRK-49369          TO WRK-OUT-PRODIA-ATTR-RCTM
                                          WRK-OUT-PROMES-ATTR-RCTM
                                          WRK-OUT-PROANO-ATTR-RCTM
               MOVE 'DATA DA PRORROGACAO DE VENCIMENTO DEVE SER PREENCHI
      -             'DA'               TO WRK-OUT-MENSA-RCTM
               MOVE '*'                TO WRK-INCONSIS
               GO TO 2281-99-FIM

           END-IF.

           INITIALIZE  WRK-AREA-BRAD1205
           MOVE WRK-OUT-PRODIA-RCTM    TO  WRK-DD-SAMD
           MOVE WRK-OUT-PROMES-RCTM    TO  WRK-MM-SAMD
           MOVE WRK-OUT-PROANO-RCTM    TO  WRK-AA-SAMD
           MOVE WRK-DT-SAMD-N          TO  WRK-DT-ENVIADA
           MOVE 'N'                    TO  WRK-OPCAO
           PERFORM 7100-CONSISTIR-DATA

           IF  RETURN-CODE          NOT EQUAL ZEROS OR
               WRK-OPCAO                EQUAL 'I'
               MOVE WRK-49369          TO WRK-OUT-PRODIA-ATTR-RCTM
                                          WRK-OUT-PROMES-ATTR-RCTM
                                          WRK-OUT-PROANO-ATTR-RCTM
               MOVE 'DATA DA PRORROGACAO DE VENCIMENTO INVALIDA'
                                       TO WRK-OUT-MENSA-RCTM
               MOVE '*'                TO WRK-INCONSIS
               GO TO 2281-99-FIM
           END-IF


           MOVE WRK-INP-DTVENC-RCTM    TO WRK-DT-AUX.
           MOVE WRK-DT-AUX-DD          TO WRK-DD-SAMD-VCTO.
           MOVE WRK-DT-AUX-MM          TO WRK-MM-SAMD-VCTO.
           MOVE WRK-DT-AUX-AA          TO WRK-AA-SAMD-VCTO.

           IF  WRK-DT-SAMD-N           LESS WRK-DT-VENCTO
               MOVE WRK-49369          TO WRK-OUT-PRODIA-ATTR-RCTM
                                          WRK-OUT-PROMES-ATTR-RCTM
                                          WRK-OUT-PROANO-ATTR-RCTM
               MOVE 'DATA DE PRORROGACAO DEVE SER MAIOR QUE A DATA DE VE
      -             'NCIMENTO ('
                                       TO WRK-OUT-MENSA-RCTM
               MOVE WRK-INP-DTVENC-RCTM
                                       TO WRK-OUT-MENSA-RCTM(62:10)
               MOVE ')'                TO WRK-OUT-MENSA-RCTM(72:1)
               MOVE '*'                TO WRK-INCONSIS
               GO TO 2281-99-FIM
           END-IF

           MOVE WRK-INP-TXJURO-RCTM    TO  WRK-VAL-ENTRADA


           PERFORM 7000-CONSISTIR-VALOR

           IF  RETURN-CODE         NOT EQUAL ZEROS OR
               WRK-VAL-SAIDA           EQUAL ZEROS
               MOVE WRK-49369          TO WRK-OUT-TXJURO-ATTR-RCTM
               MOVE SG-MENSAGEM(0141)  TO WRK-OUT-MENSA-RCTM
               MOVE '*'                TO WRK-INCONSIS
BRQ001         GO TO 2281-99-FIM
           ELSE
               MOVE WRK-VAL-SAIDA      TO WRK-OUT-TXJURO-RCTM-N
           END-IF.

BRQ001     IF  WRK-INP-LTBSPR-RCTM     NOT EQUAL SPACES
BRQ001         MOVE SPACES             TO WRK-OUT-DSBSPR-RCTM
BRQ001         IF  WRK-INP-CDBSPR-RCTM EQUAL SPACES
BRQ001             MOVE '*'            TO WRK-INCONSIS
BRQ001             MOVE WRK-49369      TO WRK-OUT-CDBSPR-ATTR-RCTM
BRQ001             MOVE 'OBRIGATORIO INFORMAR CODIGO DE BASE LEGAL PARA
BRQ001-                 'PRORROGACAO'  TO WRK-OUT-MENSA-RCTM
BRQ001             GO TO 2281-99-FIM
BRQ001         ELSE

BRQ001             IF  WRK-INP-CDBSPR-RCTM   NOT NUMERIC OR
BRQ001                 WRK-INP-CDBSPR-RCTM-N EQUAL ZEROS
BRQ001                 MOVE '*'        TO WRK-INCONSIS
BRQ001                 MOVE WRK-49369  TO WRK-OUT-CDBSPR-ATTR-RCTM
BRQ001                 MOVE 'CODIGO DE BASE LEGAL PRORROGACAO INVALIDO'
BRQ001                                 TO WRK-OUT-MENSA-RCTM
BRQ001                 GO TO 2281-99-FIM
BRQ001             ELSE
BRQ001                 MOVE WRK-209    TO WRK-OUT-CDBSPR-ATTR-RCTM
BRQ001             END-IF
BRQ001         END-IF
BRQ001     END-IF.

      *----------------------------------------------------------------*
       2281-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2282-OBTER-TOTAIS               SECTION.
      *----------------------------------------------------------------*


           INITIALIZE                     WRK-5471-ENTRADA
                                          WRK-5471-SAIDA

           MOVE '0000'                   TO WRK-5471S-COD-RETORNO
           MOVE SPACES                   TO WRK-5471S-MSG-RETORNO
BSI001     MOVE WRK-COD-USER-R           TO WRK-5471E-CFUNC-BDSCO
           MOVE LNK-IO-LTERM             TO WRK-5471E-CTERM
BSI001     MOVE WRK-INP-OPERAC-RCTM      TO WRK-5471E-OPERACAO
           MOVE WRK-INP-CODPRD-RCTM      TO WRK-5471E-PRODUTO
           MOVE WRK-INP-CODSUB-RCTM      TO WRK-5471E-SUBPRODUTO
           MOVE WRK-INP-PARCEL-RCTM      TO WRK-5471E-NPCELA-DESC-COML
           MOVE WRK-INP-DTVENC-RCTM      TO WRK-5471E-DVCTO-PCELA-DESC
           MOVE WRK-INP-VALOR-RCTM       TO WRK-5471E-VPRINC-PCELA-DESC
           MOVE WRK-INP-TAXA-RCTM        TO WRK-5471E-PTX-JURO-MES
           MOVE WRK-INP-PRODIA-RCTM-N    TO WRK-DT-AUX-DD
           MOVE WRK-INP-PROMES-RCTM-N    TO WRK-DT-AUX-MM
           MOVE WRK-INP-PROANO-RCTM-N    TO WRK-DT-AUX-AA

           INSPECT WRK-DT-AUX            REPLACING ALL '/' BY '.'

           MOVE WRK-DT-AUX            TO WRK-5471E-DVCTO-PCELA-PRROG


           INSPECT WRK-5471E-DVCTO-PCELA-DESC  REPLACING ALL '/' BY '.'.

           INITIALIZE  WRK-AREA-BRAD0496


           MOVE WRK-INP-TXJURO-RCTM    TO WRK-VALOR-0496.
           MOVE 09080305               TO WRK-TAMANHO-0496.
           MOVE SPACES                 TO WRK-SAIDA-0496.

           CALL 'BRAD0496'             USING WRK-VALOR-0496
                                             WRK-SAIDA-0496
                                             WRK-TAMANHO-0496.


           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE WRK-INP-TXJURO-RCTM-N
                                        TO WRK-5471E-PTX-JURO-MES-PRROG
           ELSE
               MOVE WRK-SAIDA-0496-N    TO WRK-5471E-PTX-JURO-MES-PRROG
           END-IF



           MOVE 'DCOM5471'             TO WRK-MODULO

           CALL WRK-MODULO          USING WRK-5471-ENTRADA
                                          WRK-5471-SAIDA
                                          ERRO-AREA
                                          WRK-SQLCA.

           EVALUATE WRK-5471S-COD-RETORNO
              WHEN '0000'
                    MOVE WRK-5471S-VALOR-JUROS  TO WRK-INP-JAUTOR-RCTM-N
                                                   WRK-OUT-JAUTOR-RCTM-N
                    MOVE WRK-5471S-VALOR-IOF    TO WRK-INP-IAUTOR-RCTM-N
                                                   WRK-OUT-IAUTOR-RCTM-N
                    MOVE WRK-5471S-TARIFA-PRROG TO WRK-INP-TACAUT-RCTM-N
                                                   WRK-OUT-TACAUT-RCTM-N
                    MOVE WRK-5471S-VALOR-ENC    TO WRK-OUT-TOTAUT-RCTM-N

              WHEN '0099'
                    MOVE 'DB2'         TO ERR-TIPO-ACESSO

                    PERFORM 9999-PROCESSAR-ROTINA-ERRO

              WHEN OTHER
                   MOVE WRK-5471S-MSG-RETORNO
                                       TO WRK-OUT-MENSA-RCTM
                   MOVE '*'            TO WRK-INCONSIS

           END-EVALUATE.

      *----------------------------------------------------------------*
       2282-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     RETIRAR VIRGULA DO CAMPO TAXA DE JUROS MENSAL - BRAD0496   *
      *----------------------------------------------------------------*
       2283-CONSISTIR-TXJURO-MENSAL    SECTION.
      *----------------------------------------------------------------*

           INITIALIZE  WRK-AREA-BRAD0496


           MOVE WRK-INP-TXJURO-RCTM    TO WRK-VALOR-0496.
           MOVE 09080305               TO WRK-TAMANHO-0496.
           MOVE SPACES                 TO WRK-SAIDA-0496.

           CALL 'BRAD0496'             USING WRK-VALOR-0496
                                             WRK-SAIDA-0496
                                             WRK-TAMANHO-0496.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE '*'                TO WRK-INCONSIS
               MOVE WRK-49369          TO WRK-OUT-TXJURO-ATTR-RCTM
               MOVE SG-MENSAGEM(141)   TO WRK-OUT-MENSA-RCTM
               MOVE SPACES             TO WRK-INP-LITAUT-RCTM
                                          WRK-OUT-LITAUT-RCTM
                                          WRK-INP-CODAUT-RCTM
                                          WRK-OUT-CODAUT-RCTM
                                          WRK-INP-DESAUT-RCTM
                                          WRK-OUT-DESAUT-RCTM
               GO TO 2283-99-FIM
           ELSE
               MOVE WRK-SAIDA-0496-N   TO WRK-TAXA-JURO-MES

                                          WRK-OUT-TXJURO-RCTM-N

               IF  WRK-INP-TPJUR-RCTM      EQUAL 2
                   IF  WRK-TAXA-JURO-MES   LESS WRK-INP-TSMIN-RCTM-N OR
                       WRK-TAXA-JURO-MES   GREATER WRK-INP-TSMAX-RCTM-N
                       MOVE '*'            TO WRK-INCONSIS
                       MOVE WRK-49369      TO WRK-OUT-TXJURO-ATTR-RCTM
                       MOVE SPACES         TO WRK-INP-LITAUT-RCTM
                                              WRK-OUT-LITAUT-RCTM
                                              WRK-INP-CODAUT-RCTM
                                              WRK-OUT-CODAUT-RCTM
                                              WRK-INP-DESAUT-RCTM
                                              WRK-OUT-DESAUT-RCTM

                       IF  WRK-TAXA-JURO-MES < WRK-INP-TSMIN-RCTM-N
                           MOVE WRK-MSG-4473-1 TO WRK-OUT-MENSA-RCTM
                       END-IF

                       IF  WRK-TAXA-JURO-MES > WRK-INP-TSMAX-RCTM-N
                           MOVE WRK-MSG-4473-2 TO WRK-OUT-MENSA-RCTM
                       END-IF


                       GO TO 2283-99-FIM
                   END-IF
               END-IF
               END-IF.

      *----------------------------------------------------------------*
       2283-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2284-CONSISTIR-INTERVALOR-JURO  SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-INP-TXMIN-RCTM     TO WRK-VALOR-0496-2.
           MOVE 06050203               TO WRK-TAMANHO-0496-2.
           MOVE SPACES                 TO WRK-SAIDA-0496-2.

           CALL 'BRAD0496'             USING WRK-VALOR-0496-2
                                             WRK-SAIDA-0496-2
                                             WRK-TAMANHO-0496-2.


           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE ZEROS              TO WRK-TAXA-JURO-MIN
           ELSE
               MOVE WRK-SAIDA-0496-2-N TO WRK-TAXA-JURO-MIN
           END-IF.

           MOVE WRK-INP-TXMAX-RCTM     TO WRK-VALOR-0496-2.
           MOVE 06050203               TO WRK-TAMANHO-0496-2.
           MOVE SPACES                 TO WRK-SAIDA-0496-2.

           CALL 'BRAD0496'             USING WRK-VALOR-0496-2
                                             WRK-SAIDA-0496-2
                                             WRK-TAMANHO-0496-2.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE ZEROS              TO WRK-TAXA-JURO-MAX
           ELSE
               MOVE WRK-SAIDA-0496-2-N TO WRK-TAXA-JURO-MAX
           END-IF.


PRIME      PERFORM 2287-VER-AG-DEPTO.

           IF (WRK-TAXA-JURO-MES       LESS WRK-TAXA-JURO-MIN OR
               WRK-TAXA-JURO-MES       GREATER WRK-TAXA-JURO-MAX )
PRIME      AND WRK-5549-S-AG-DEPTO     EQUAL 'A'
               IF  WRK-INP-LITAUT-RCTM EQUAL SPACES
               AND WRK-INP-TPJUR-RCTM  EQUAL 2
                   MOVE 'COD.AUTORIZ:'     TO WRK-OUT-LITAUT-RCTM
                   MOVE '*'            TO WRK-INCONSIS
                   MOVE SG-MENSAGEM(142)  TO WRK-OUT-MENSA-RCTM
                   MOVE WRK-49361         TO WRK-OUT-CODAUT-ATTR-RCTM
                   IF  WRK-TAXA-JURO-MES GREATER WRK-TAXA-JURO-MAX
                       MOVE 'TAXA ACIMA DA REDE - NECESSARIO AUTORIZACAO
      -                     ''         TO WRK-OUT-MENSA-RCTM
                   END-IF
               ELSE
                   MOVE WRK-209        TO WRK-OUT-CODAUT-ATTR-RCTM
               END-IF
           ELSE
               MOVE SPACES             TO WRK-INP-LITAUT-RCTM
                                          WRK-OUT-LITAUT-RCTM

                                          WRK-INP-CODAUT-RCTM
                                          WRK-OUT-CODAUT-RCTM
                                          WRK-INP-DESAUT-RCTM
                                          WRK-OUT-DESAUT-RCTM
           END-IF.
           IF  WRK-INP-TPJURO-RCTM     EQUAL 'TXREDE' OR 'TXAESP'
               IF  WRK-TAXA-JURO-MES   LESS WRK-TAXA-JURO-MAX
                   MOVE 'TXAESP'       TO WRK-INP-TPJURO-RCTM
                                          WRK-OUT-TPJURO-RCTM
               ELSE
                   MOVE 'TXREDE'       TO WRK-INP-TPJURO-RCTM
                                          WRK-OUT-TPJURO-RCTM
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2284-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2285-ACESSAR-DCOM5424           SECTION.

      *----------------------------------------------------------------*

           INITIALIZE WRK-AREA-DCOM5424
                      ERRO-AREA
                      WRK-SQLCA.

           MOVE WRK-COD-USER-R         TO WRK-5424E-CFUNC-BDSCO.
           MOVE LNK-IO-LTERM           TO WRK-5424E-CTERM.
           MOVE WRK-INP-OPERAC-RCTM    TO WRK-5424E-OPERACAO.
           MOVE WRK-INP-CODPRD-RCTM    TO WRK-5424E-CPRODT.
           MOVE WRK-INP-CODSUB-RCTM    TO WRK-5424E-CSPROD-DESC-COML.
           MOVE 'DCOM5424'             TO WRK-MODULO.

           CALL WRK-MODULO             USING WRK-AREA-DCOM5424
                                             ERRO-AREA
                                             WRK-SQLCA.

           EVALUATE WRK-5424S-COD-RETORNO
              WHEN ZEROS
                   MOVE WRK-5424S-CIDTFD-TX-REDE
                                       TO WRK-INP-FLAG-RCTM

                                          WRK-OUT-FLAG-RCTM
                   MOVE WRK-5424S-CIDTFD-TX-MIN-VLR
                                       TO WRK-INP-TSMIN-RCTM-N
                                          WRK-OUT-TSMIN-RCTM-N
                   MOVE WRK-5424S-CIDTFD-TX-MAX-VLR
                                       TO WRK-INP-TSMAX-RCTM-N
                                          WRK-OUT-TSMAX-RCTM-N
                   IF  WRK-OUT-FLAG-RCTM    EQUAL 'S' AND
                      (WRK-OUT-TPJUR-RCTM   NOT EQUAL 3)
                       MOVE WRK-225         TO WRK-OUT-MINMAX-ATTR-RCTM
                   ELSE
                       MOVE WRK-229         TO WRK-OUT-MINMAX-ATTR-RCTM
                   END-IF

              WHEN 99
                   MOVE 'DB2'                 TO ERR-TIPO-ACESSO
                   PERFORM 9999-PROCESSAR-ROTINA-ERRO
              WHEN OTHER
                   MOVE '*'                   TO WRK-INCONSIS
                   MOVE WRK-5424S-MSG-RETORNO TO WRK-OUT-MENSA-RCTM
                   MOVE SPACES                TO WRK-OUT-FLAG-RCTM

                   MOVE ZEROS                 TO WRK-OUT-TSMIN-RCTM-N
                                                 WRK-OUT-TSMAX-RCTM-N
           END-EVALUATE.

      *----------------------------------------------------------------*
       2285-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2286-CONSISTIR-TAXA             SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WRK-5151-ENTRADA
                      WRK-5151-SAIDA
                      ERRO-AREA
                      WRK-SQLCA.

           MOVE 'DCOM5151'               TO WRK-MODULO.
           MOVE WRK-COD-USER-R           TO WRK-5151E-CFUNC-BDSCO
           MOVE LNK-IO-LTERM             TO WRK-5151E-CTERM
BSI001     MOVE WRK-INP-OPERAC-RCTM      TO WRK-5151E-OPERACAO.

BSI001     MOVE WRK-INP-CNPJ1-RCTM       TO WRK-CNPJ-1.
BSI001     MOVE WRK-INP-CNPJ2-RCTM       TO WRK-CNPJ-2.
BSI001     MOVE WRK-INP-CNPJ3-RCTM       TO WRK-CNPJ-3.
BSI001     MOVE WRK-NRO-CNPJ             TO WRK-5151E-CCNPJ-CPF.
BSI001     MOVE WRK-INP-CNPJ4-RCTM       TO WRK-5151E-CFLIAL-CNPJ.
           MOVE WRK-INP-DTVENC-RCTM      TO WRK-5151E-DVCTO-ANTE
           MOVE WRK-INP-PRODIA-RCTM      TO WRK-5151E-DVCTO-NOVA(1:2)
           MOVE '.'                      TO WRK-5151E-DVCTO-NOVA(3:1)
           MOVE WRK-INP-PROMES-RCTM      TO WRK-5151E-DVCTO-NOVA(4:2)
           MOVE '.'                      TO WRK-5151E-DVCTO-NOVA(6:1)
           MOVE WRK-INP-PROANO-RCTM      TO WRK-5151E-DVCTO-NOVA(7:4)

           INITIALIZE  WRK-AREA-BRAD0496-3

           MOVE WRK-INP-VALOR-RCTM     TO WRK-VALOR-0496-3.
           MOVE 17131102               TO WRK-TAMANHO-0496-3.
           MOVE SPACES                 TO WRK-SAIDA-0496-3.

           CALL 'BRAD0496'             USING WRK-VALOR-0496-3
                                             WRK-SAIDA-0496-3
                                             WRK-TAMANHO-0496-3.


           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE '*'                TO WRK-INCONSIS
               MOVE SG-MENSAGEM(141)   TO WRK-OUT-MENSA-RCTM
               GO TO 2286-99-FIM
           ELSE
               MOVE WRK-SAIDA-0496-N-3 TO WRK-5151E-VPRINC-PCELA
           END-IF


           CALL WRK-MODULO             USING WRK-5151-ENTRADA
                                             WRK-5151-SAIDA
                                             ERRO-AREA
                                             WRK-SQLCA.


           EVALUATE WRK-5151S-COD-RETORNO
              WHEN ZEROS
                   MOVE WRK-5151S-CSGL-TPO-TX-JURO
BSI001                                        TO WRK-OUT-TXJURO-RCTM
              WHEN '0099'

                   MOVE 'DB2'                 TO ERR-TIPO-ACESSO
                   PERFORM 9999-PROCESSAR-ROTINA-ERRO
              WHEN OTHER
                   MOVE '*'                   TO WRK-INCONSIS
                   MOVE WRK-5151S-MSG-RETORNO TO WRK-OUT-MENSA-RCTM

           END-EVALUATE.

      *----------------------------------------------------------------*
       2286-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

PRIME *----------------------------------------------------------------*
      *     VERIFICAR SE O USUARIO E AGENCIA OU DEPTO                  *
PRIME *----------------------------------------------------------------*
       2287-VER-AG-DEPTO               SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WRK-5549-ENTRADA
                      WRK-5549-SAIDA.


           MOVE WRK-COD-USER-R         TO WRK-5549-E-CFUNC-BDSCO.
           MOVE WRK-COD-DEPTO-N        TO WRK-5549-E-DEPTO.
           MOVE 'DCOM5549'             TO WRK-MODULO.

           CALL WRK-MODULO             USING WRK-5549-ENTRADA
                                             WRK-5549-SAIDA
                                             ERRO-AREA
                                             WRK-SQLCA.

           IF  WRK-5549-S-COD-RETORNO      NOT EQUAL ZEROS
               IF  WRK-5549-S-COD-RETORNO  EQUAL '0099'
                   PERFORM 9999-PROCESSAR-ROTINA-ERRO
               END-IF
               MOVE '*'                    TO WRK-INCONSIS
               MOVE WRK-5549-S-MSG-RETORNO TO WRK-OUT-MENSA-RCTM
               MOVE WRK-49361              TO WRK-OUT-TXJURO-ATTR-RCTM
            END-IF.

      *----------------------------------------------------------------*
       2287-99-FIM.                    EXIT.
PRIME *----------------------------------------------------------------*


BRQ001*----------------------------------------------------------------*
BRQ001 2288-CONSISTIR-BASE-PRORROG     SECTION.
BRQ001*----------------------------------------------------------------*
BRQ001
BRQ001     INITIALIZE WRK-5218-ENTRADA
BRQ001                WRK-5218-SAIDA
BRQ001                ERRO-AREA
BRQ001                WRK-SQLCA.
BRQ001
BRQ001     MOVE WRK-COD-USER-R         TO WRK-5218E-CFUNC-BDSCO.
BRQ001     MOVE LNK-IO-LTERM           TO WRK-5218E-CTERM.
BRQ001     MOVE 'U'                    TO WRK-5218E-TPRETORNO
BRQ001     MOVE WRK-INP-CDBSPR-RCTM-N  TO WRK-5218E-CDBASE-RURAL
BRQ001     MOVE 'DCOM5218'             TO WRK-MODULO.
BRQ001
BRQ001     CALL WRK-MODULO             USING WRK-5218-ENTRADA
BRQ001                                       WRK-5218-SAIDA
BRQ001                                       ERRO-AREA
BRQ001                                       WRK-SQLCA.
BRQ001

BRQ001     EVALUATE WRK-5218S-COD-RETORNO
BRQ001         WHEN ZEROS
BRQ001              STRING ' - '
BRQ001                      WRK-5218S-DESCRICAO(1)(1:52)
BRQ001              DELIMITED BY SIZE INTO WRK-OUT-DSBSPR-RCTM
BRQ001
BRQ001         WHEN 0099
BRQ001              PERFORM 9999-PROCESSAR-ROTINA-ERRO
BRQ001
BRQ001         WHEN OTHER
BRQ001              MOVE WRK-5218S-MSG-RETORNO
BRQ001                                 TO WRK-OUT-MENSA-RCTM
BRQ001              MOVE '*'           TO WRK-INCONSIS
BRQ001              MOVE WRK-49369     TO WRK-OUT-CDBSPR-ATTR-RCTM
BRQ001              MOVE SPACES        TO WRK-OUT-DSBSPR-RCTM
BRQ001     END-EVALUATE.
BRQ001
BRQ001*----------------------------------------------------------------*
BRQ001 2288-99-FIM.                    EXIT.
BRQ001*----------------------------------------------------------------*
BRQ001

BRQ001*----------------------------------------------------------------*
BRQ001 2290-ACESSAR-DCOM3929           SECTION.
BRQ001*----------------------------------------------------------------*
BRQ001
BRQ001     PERFORM 1263-GRAVAR-DBTEMP.
BRQ001
BRQ001     INITIALIZE WRK-COMU-AREA-3929.
BRQ001
BRQ001     MOVE LENGTH                 OF WRK-COMU-AREA-3929
BRQ001                                 TO WRK-COMU-LL-3929.
BRQ001     MOVE WRK-INP-TMSTAMP-RCTM   TO WRK-COMU-TMSTAMP-3929.
BRQ001     MOVE 'DCOM1803'             TO WRK-COMU-TRANS-3929.
BRQ001     MOVE 'DCOM3929'             TO WRK-TELA.
BRQ001     MOVE WRK-CHNG               TO WRK-FUNCAO
BRQ001     MOVE WRK-COMU-AREA-3929     TO WRK-MENSAGEM.
BRQ001
BRQ001*----------------------------------------------------------------*
BRQ001 2290-99-FIM.                    EXIT.
BRQ001*----------------------------------------------------------------*

      *----------------------------------------------------------------*

       2300-PROCESSAR-BRAD0660         SECTION.
      *----------------------------------------------------------------*

           MOVE LENGTH OF WRK-660-DCOMRCTM TO WRK-660-LL-RCTM.
           MOVE LENGTH OF WRK-OUTPUT-RCTM  TO WRK-OUT-LL-RCTM
                                              WRK-660-ZZ-RCTM.

           CALL 'BRAD0660'             USING WRK-OUTPUT-RCTM
                                             WRK-660-DCOMRCTM.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE RETURN-CODE        TO WRK-RETURN-CODE
               MOVE 10                 TO WRK-LOCAL-ERRO
               MOVE WRK-ERRO-BRAD0660  TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE WRK-ISRT               TO WRK-FUNCAO
           MOVE WRK-OUTPUT-RCTM        TO WRK-MENSAGEM.


      *----------------------------------------------------------------*
       2300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           GOBACK.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       7000-CONSISTIR-VALOR            SECTION.
      *----------------------------------------------------------------*

           CALL 'BRAD0496'             USING WRK-VAL-ENTRADA
                                             WRK-VAL-SAIDA
                                             WRK-VAL-TAMANHO.


      *----------------------------------------------------------------*
       7000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       7100-CONSISTIR-DATA             SECTION.
      *----------------------------------------------------------------*

           CALL  'BRAD1205'            USING WRK-AREA-BRAD1205
                                             WRK-MENSAGEM-1205.

      *----------------------------------------------------------------*
       7100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9999-PROCESSAR-ROTINA-ERRO      SECTION.
      *----------------------------------------------------------------*

           IF  ERR-MODULO              NOT EQUAL SPACES AND LOW-VALUES

               MOVE 'DCOM1803'         TO ERR-PGM
           ELSE
               MOVE 'SENHAS02'         TO ERR-PGM
               MOVE 'DCOM1803'         TO ERR-MODULO
           END-IF.

           MOVE WRK-COD-USER           TO ERR-COD-USER
           MOVE WRK-COD-DEPTO          TO ERR-COD-DEPTO

           IF  ERR-TIPO-ACESSO         EQUAL 'DB2'
               CALL 'BRAD7100'         USING LNK-IO-PCB
                                             ERRO-AREA
                                             LNK-ALT-PCB
                                             WRK-SQLCA
           ELSE
               CALL 'BRAD7100'         USING LNK-IO-PCB
                                             ERRO-AREA
                                             LNK-ALT-PCB
           END-IF.

           PERFORM 3000-FINALIZAR.


      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
