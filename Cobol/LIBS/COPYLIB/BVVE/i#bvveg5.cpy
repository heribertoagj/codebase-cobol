      *----------------------------------------------------------------*
      *  SISTEMA : BVVE - CARTAO DE CREDITO / VENDEDOR                 *
      *  ARQUIVO : DADOS DO ENVIO PARA ATUALIZACAO DA TABELA DE        *
      *            SOLICITACOES DE  TRAVA/DESTRAVA DE DOMICILIO        *
      *  LRECL   : 062 (FB)                                            *
      *  NOME INC: I#BVVEG5                                            *
      *  DATA    : 20/07/2008                                          *
      *----------------------------------------------------------------*
       01  BVVEG5-REG.
           05  BVVEG5-DATA-ENVIO              PIC 9(009)     COMP-3.
           05  BVVEG5-HORA-ENVIO              PIC 9(007)     COMP-3.
           05  BVVEG5-VERSAO-ARQ              PIC 9(009)     COMP-3.
           05  BVVEG5-PONTO-VENDA             PIC 9(009)     COMP-3.
           05  BVVEG5-CNPJCPF-NRO             PIC 9(009)     COMP-3.
           05  BVVEG5-CNPJ-FILIAL             PIC 9(005)     COMP-3.
           05  BVVEG5-CNPJCPF-CTRL            PIC 9(002)     COMP-3.
           05  BVVEG5-TIPO-OPER               PIC X(001).
           05  BVVEG5-BANCO                   PIC 9(003)     COMP-3.
           05  BVVEG5-AGENCIA                 PIC 9(005)     COMP-3.
           05  BVVEG5-CONTA-NRO               PIC 9(013)     COMP-3.
           05  BVVEG5-CONTA-DIG               PIC X(001).
           05  BVVEG5-IND-TRAVA               PIC X(001).
           05  BVVEG5-DTINIC-TRAVA            PIC 9(009)     COMP-3.
           05  BVVEG5-DTFIM-TRAVA             PIC 9(009)     COMP-3.
           05  BVVEG5-CEMPR-INC               PIC 9(05)      COMP-3.
           05  BVVEG5-CDEPDC                  PIC 9(05)      COMP-3.
           05  BVVEG5-TPO-BANDEIRA            PIC X(002).
      *----------------------------------------------------------------*
      * 001-005 DATA DO ENVIO DO ARQUIVO PELO BANCO (AAAAMMDD)
      * 006-009 HORARIO DO ENVIO PELO BANCO (HHMMSS)
      * 010-014 VERSAO DO ARQUIVO ENVIADO PELO BANCO
      * 015-019 NUMERO DO PONTO DE VENDA=CODIGO DO ESTABELECIMENTO COML
      * 020-024 NUMERO DO CNPJ/CPF DO ESTABELECIMENTO
      * 025-027 FILIAL DO CNPJ DO ESTABELECIMENTO
      * 028-029 CONTROLE DO CNPJ/CPF DO ESTABELECIMENTO
      * 030-030 TIPO DE OPERACAO C=CREDITO D=DEBITO
      * 031-032 BANCO DA CONTA CORRENTE DO ESTABELECIMENTO
      * 033-035 AGENCIA DA CONTA CORRENTE DO ESTABELECIMENTO
      * 036-042 NUMERO DA CONTA CORRENTE DO ESTABELECIMENTO
      * 043-043 DIGITO DA CONTA CORRENTE DO ESTABELECIMENTO
      * 044-044 INDICADOR DE TRAVA T=TRAVA D=DESTRAVA
      * 045-049 DATA DE INICIO DE TRAVA SOLICITADO PELO BANCO AAAAMMDD
      * 050-054 DATA DE TERMINO DE TRAVA SOLICITADO PELO BANCO AAAAMMDD
      * 055-057 CODIGO DA EMPRESA/BANCO INCORPORADA(O)
      * 058-060 CODIGO DA DEPENDENCIA BRADESCO (AGENCIA/DEPTO)
      * 061-062 TIPO DE BANDEIRA: VISA OU MASTERCARD
      *----------------------------------------------------------------*
