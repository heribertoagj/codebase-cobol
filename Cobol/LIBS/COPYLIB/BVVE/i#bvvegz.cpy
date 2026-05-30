      *----------------------------------------------------------------*
      *  SISTEMA : BVVE - CARTAO DE CREDITO / VENDEDOR                 *
      *  ARQUIVO : DETALHES DO ARQUIVO DE RETORNO DE SOLICITACOES DE   *
      *            TRANSFERENCIA  DE DOMICILIO BANCARIO COM DADOS DO   *
      *            HEADER E DO TRAILER                                 *
      *  LRECL   : 147 (FB)                                            *
      *  NOME INC: I#BVVEGZ                                            *
      *  DATA    : 29/07/2008                                          *
      *----------------------------------------------------------------*
       01  BVVEGZ-REG.
           05  BVVEGZ-BANCO-ENVIO             PIC 9(003)     COMP-3.
           05  BVVEGZ-DATA-ENVIO              PIC 9(009)     COMP-3.
           05  BVVEGZ-HORA-ENVIO              PIC 9(007)     COMP-3.
           05  BVVEGZ-VERSAO-REDECARD         PIC 9(009)     COMP-3.
           05  BVVEGZ-RAZAO-SOCIAL            PIC X(028).
           05  BVVEGZ-PONTO-VENDA             PIC 9(009)     COMP-3.
           05  BVVEGZ-CNPJCPF-ESTAB.
               10  BVVEGZ-CNPJCPF-NRO         PIC 9(009)     COMP-3.
               10  BVVEGZ-CNPJ-FILIAL         PIC 9(005)     COMP-3.
               10  BVVEGZ-CNPJCPF-CTRL        PIC 9(002).
           05  BVVEGZ-TIPO-OPER               PIC X(001).
           05  BVVEGZ-AGENCIA                 PIC 9(005)     COMP-3.
           05  BVVEGZ-CONTA-NRO               PIC 9(013)     COMP-3.
           05  BVVEGZ-CONTA-DIG               PIC X(001).
           05  BVVEGZ-IND-TRAVA               PIC X(001).
           05  BVVEGZ-DTENVIO-BANCO           PIC 9(009)     COMP-3.
           05  BVVEGZ-PV-CENTRALIZ            PIC 9(009)     COMP-3.
           05  BVVEGZ-SIGLA-PRODUTO           PIC X(002).
           05  BVVEGZ-COD-RETORNO             PIC 9(005)     COMP-3.
           05  BVVEGZ-MSG-ERRO                PIC X(060).
      *----------------------------------------------------------------*
      * 001-002 CODIGO DO BANCO QUE ESTA ENVIANDO ARQUIVOS
      * 003-007 DATA DE ENVIO PELA REDECARD (AAAAMMDD)
      * 008-011 HORA DE ENVIO PELA REDECARD (HHMMSS)
      * 012-016 VERSAO DO ARQUIVO DA REDECARD
      * 017-044 RAZAO SOCIAL DO ESTABELECIMENTO
      * 045-049 NUMERO DO PONTO DE VENDA DO ESTABELECIMENTO
      * 050-054 NUMERO DO CNPJ/CPF DO ESTABELECIMENTO
      * 055-057 FILIAL DO CNPJ DO ESTABELECIMENTO
      * 058-059 CONTROLE DO CNPJ/CPF DO ESTABELECIMENTO
      * 060-060 TIPO DE OPERACAO C=CREDITO D=DEBITO
      * 061-063 AGENCIA DA CONTA CORRENTE DO ESTABELECIMENTO
      * 064-070 NUMERO DA CONTA CORRENTE DO ESTABELECIMENTO
      * 071-071 DIGITO DA CONTA CORRENTE DO ESTABELECIMENTO
      * 072-072 INDICADOR DE TRAVA T=TRAVA D=DESTRAVA
      * 073-077 DATA DO REGISTRO ENVIADO PELO BANCO AAAAMMDD
      * 078-082 PONTO DE VENDA CENTRALIZADOR DOS CREDITO
      * 083-084 SIGLA DO PRODUTO
      *         CR - MASTERCARD CREDITO
      *         DB - MASTERCARD DEBITO
      *         VC - VISA CREDITO
      *         VD - VISA DEBITO
      * 085-087 CODIGO DE RETORNO/ERRO
      * 088-147 MENSAG DE ERRO
      *----------------------------------------------------------------*
