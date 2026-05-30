      *---------------------------------------------------------------*
      * I#BVVEIG -  BRADESCO - INFORMACOES GERENCIAIS  - TAMANHO: 140 *
      *---------------------------------------------------------------*
      * 23/06/2009 - CRIACAO.                                         *
      *---------------------------------------------------------------*
      * OBSERVACOES:                                                  *
      * 01. DADOS RECEBIDOS PARA ENVIO DE INFORMACOES GERENCIAIS AO   *
      *     SISTEMA SIGB, COM RESULTADO DA CONSISTENCIA.              *
      * 02. MENSAGENS:                                                *
      *     001-AGENCIA, CONTA OU CPF/CNPJ: CONTEUDO NAO VALIDO.      *
      *     002-CPF/CNPJ: CONTROLE NAO CONFERE OU NUMERO VICIADO.     *
      *     003-CLIENTE NAO POSSUI CONTA CORRENTE.                    *
      *     004-CLIENTE NAO TITULAR DA CONTA.                         *
      *     005-CONTA CORRENTE INATIVA.                               *
      *     006-CONTA CORRENTE ATIVA.                                 *
      *     007-DADOS ENVIADOS PARA SISTEMA SIGB.                     *
      *---------------------------------------------------------------*

       01      REG-BRINGER.
        03     BIG-RECEBIDO.
         05    BIG-NUMAGEN.
          10   BIG-NUMAGEN-N           PIC  9(04).
          10   BIG-NUMAGEN-S           REDEFINES
               BIG-NUMAGEN-N           PIC S9(04).
      *        001-004                 NUMERO AGENCIA.
         05    BIG-NUMCONT.
          10   BIG-NUMCONT-N           PIC  9(10).
          10   BIG-NUMCONT-S           REDEFINES
               BIG-NUMCONT-N           PIC S9(10).
      *        005-014                 NUMERO CONTA.
         05    BIG-CNP.
          10   BIG-CNPNUME.
           15  BIG-CNPNUME-N           PIC  9(09).
      *        015-023                 CAD.NACIONAL PESSOA - NUMERO.
          10   BIG-CNPFILI.
           15  BIG-CNPFILI-N           PIC  9(04).
      *        024-027                 CAD.NACIONAL PESSOA - FILIAL.
          10   BIG-CNPCONT.
           15  BIG-CNPCONT-N           PIC  9(02).
      *        028-029                 CAD.NACIONAL PESSOA - CONTROLE.
         05    BIG-CNP-N               REDEFINES
               BIG-CNP                 PIC  9(15).
         05    BIG-CNP-S               REDEFINES
               BIG-CNP                 PIC S9(15).
         05    BIG-ORIGEM              PIC  X(08).
      *        030-037                 ORIGEM DADOS (REDECARD, VISANET
      *                                OU OUTRAS).
         05    BIG-CODCART             PIC  9(03).
      *        038-040                 CODIGO CARTEIRA.
         05    BIG-RESER01             PIC  X(40).
      *        041-080                 RESERVA.
        03     BIG-PROCESSADO.
         05    BIG-NUMAGEN-CAD         PIC  9(05)  COMP-3.
      *        081-083                 NUMERO AGENCIA CADASTRADO.
         05    BIG-CODRAZA-CAD         PIC  9(05)  COMP-3.
      *        084-086                 CODIGO RAZAO CADASTRADO.
         05    BIG-NUMCONT-CAD         PIC  9(07)  COMP-3.
      *        087-090                 NUMERO CONTA CADASTRADO.
         05    BIG-DIGCONT-CAD         PIC  X(01).
      *        091-091                 DIGITO CONTA CADASTRADO.
         05    BIG-CNPNUME-CAD         PIC  9(09)  COMP-3.
      *        092-096                 CNP CADASTRADO - NUMERO.
         05    BIG-CNPFILI-CAD         PIC  9(04).
      *        097-100                 CNP CADASTRADO - FILIAL.
         05    BIG-CNPCONT-CAD         PIC  9(02).
      *        101-102                 CNP CADASTRADO - CONTROLE.
         05    BIG-RESER02             PIC  X(35).
      *        103-137                 RESERVA.
         05    BIG-CODMENS             PIC  9(03).
      *        138-140                 CODIGO MENSAGEM.
