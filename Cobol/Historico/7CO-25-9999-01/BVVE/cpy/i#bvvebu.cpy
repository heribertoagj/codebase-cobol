      *---------------------------------------------------------------*
      *    CONSTITUICAO DE GARANTIA DE ORPAG'S POR SALDO DEVEDOR      *
      *    I#BVVEBU - ARQUIVO RECEBIDO DO GRAN COM VALOR PARA MARCACAO*
      *    TAMANHO: 700                                               *
      *---------------------------------------------------------------*
       01 BVVEBU-REGISTRO.
           03 BVVEBU-TP-REG               PIC X(001)      VALUE SPACES.
           03 BVVEBU-HEADER.
              05 BVVEBU-HD-DATA           PIC X(010)      VALUE SPACES.
              05 BVVEBU-HD-HORA           PIC X(008)      VALUE SPACES.
              05 BVVEBU-HD-ROTINA         PIC X(004)      VALUE SPACES.
              05 BVVEBU-HD-ORIGEM         PIC X(001)      VALUE SPACES.
              05 FILLER                   PIC X(676)      VALUE SPACES.
           03 BVVEBU-DETALHE REDEFINES    BVVEBU-HEADER.
              05 BVVEBU-DET-CNPJ.
7C2511     07 BVVEBU-DET-CNPJN      PIC X(009).
7C2511     07 BVVEBU-DET-FILIAL     PIC X(004).
                 07 BVVEBU-DET-DIGCN      PIC 9(002).
              05 FILLER      REDEFINES    BVVEBU-DET-CNPJ.
                 07 BVVEBU-DET-CPF.
                    09 BVVEBU-DET-CPFN    PIC 9(009).
                    09 BVVEBU-DET-DIG     PIC 9(002).
                 07 FILLER                PIC X(004).
              05 BVVEBU-DET-TP-PSSOA      PIC X(001).
              05 BVVEBU-DET-AGENCIA.
                 07 BVVEBU-DET-AGEN       PIC 9(005).
              05 BVVEBU-DET-CONTA.
                 07 BVVEBU-DET-CONTAN     PIC 9(007).
              05 BVVEBU-DET-PRODUTO.
                 07 BVVEBU-DET-PRODN      PIC 9(002).
              05 BVVEBU-DET-CPROP-ESQM    PIC X(003).
              05 BVVEBU-DET-PRAZO.
                 07 BVVEBU-DET-PRAZON     PIC 9(004).
              05 BVVEBU-DET-CENTRO-CUS    PIC X(004).
              05 BVVEBU-DET-BCO           PIC 9(003).
              05 BVVEBU-DET-AG-CTR-X.
                 07 BVVEBU-DET-AG-CTR     PIC 9(005).
              05 BVVEBU-DET-CONTA-CTR-X.
                 07 BVVEBU-DET-CONTA-CTR  PIC 9(013).
              05 BVVEBU-DET-CART-CTR      PIC X(003).
              05 BVVEBU-DET-CONTRATO-X.
                 07 BVVEBU-DET-CONTRATO   PIC 9(017).
              05 BVVEBU-DET-CPSSOA-JURID-CTR
                                          PIC 9(010).
              05 BVVEBU-DET-CTPO-CTR-X.
                 07 BVVEBU-DET-CTPO-CTR   PIC 9(003).
              05 BVVEBU-DET-NSEQ-CTR-X.
                 07 BVVEBU-DET-NSEQ-CTR   PIC 9(010).
              05 BVVEBU-DET-CATALOGO      PIC 9(003).
              05 BVVEBU-DET-DT-VTO-CTR-X.
                 07 BVVEBU-DET-DT-VTO-CTR PIC 9(008).
              05 BVVEBU-DET-VR-EXGDO-X.
                 07 BVVEBU-DET-VR-EXGDO   PIC 9(015)V9(002).
              05 BVVEBU-DET-COD-RET       PIC 9(002).
              05 BVVEBU-DET-ERRO.
                 07 BVVEBU-DET-ERROTXT    PIC X(100) OCCURS 5 TIMES.
              05 BVVEBU-DET-CPOS-BRCO     PIC X(001).
              05 BVVEBU-DET-VR-LIMITE-X.
                 07 BVVEBU-DET-VR-LIMITE  PIC 9(015)V9(002).
              05 BVVEBU-DET-SLDO-DEVD-X.
                 07 BVVEBU-DET-SLDO-DEVD  PIC 9(015)V9(002).
              05 BVVEBU-DET-IND-VRLOR     PIC 9(001).
              05 BVVEBU-DET-DT-INI-CTR-X.
                 07 BVVEBU-DET-DT-INI-CTR PIC 9(008).
              05 BVVEBU-DET-VR-BLOQUEADO.
                 07 BVVEBU-DET-VR-BLOQ    PIC 9(015)V9(002).
              07 BVVEBU-DET-STATUS        PIC X(001).
              05 BVVEBU-DET-CBANDE        PIC 9(003) COMP-3.
            03 BVVEBU-TRAILLER REDEFINES BVVEBU-HEADER.
               05 BVVEBU-TR-QTD-REG       PIC 9(009).
               05 FILLER                  PIC X(690).
      *
      * BVVEBU-DET-STATUS    : 0-BLOQUEIO COMPLETO
      *                        1-BLOQUEIO PARCIAL
      *                        2-CONTRATO ENCERRADO
      *                        3-REGISTRO INCONSISTENTE
      * BVVEBU-DET-IND-VRLOR - (1 - SALDO DEVEDOR 2 - LIMITE)
      * BVVEBU-DET-DT-VTO-CTR: DDMMAAAA
      * BVVEBU-DET-DT-INI-CTR: DDMMAAAA
