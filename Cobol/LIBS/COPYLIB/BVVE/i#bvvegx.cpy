      *----------------------------------------------------------------*
      *  SISTEMA : BVVE - CARTAO DE CREDITO / VENDEDOR                 *
      *  ARQUIVO : DETALHES DO ARQUIVO DE RETORNO DE SOLICITACOES DE   *
      *            TRAVA/DESTRAVA DE DOMICILIO BANCARIO COM DADOS DO   *
      *            HEADER E DO TRAILER                                 *
      *  LRECL   : 153 (FB)                                            *
      *  NOME INC: I#BVVEGX                                            *
      *  DATA    : 28/07/2008                                          *
      *----------------------------------------------------------------*
       01  BVVEGX-REG.
           05  BVVEGX-BANCO-ENVIO             PIC 9(003)     COMP-3.
           05  BVVEGX-DATA-PROCES             PIC 9(009)     COMP-3.
           05  BVVEGX-HORA-PROCES             PIC 9(007)     COMP-3.
           05  BVVEGX-VERSAO-BANCO            PIC 9(009)     COMP-3.
           05  BVVEGX-VERSAO-REDECARD         PIC 9(009)     COMP-3.
           05  BVVEGX-PONTO-VENDA             PIC 9(009)     COMP-3.
           05  BVVEGX-CNPJ-ESTAB.
               10  BVVEGX-ECNPJ-NRO           PIC 9(008).
               10  BVVEGX-ECNPJ-FILIAL        PIC 9(004).
               10  BVVEGX-ECNPJ-CTRL          PIC 9(002).
           05  BVVEGX-CPF-ESTAB REDEFINES BVVEGX-CNPJ-ESTAB.
               10  BVVEGX-ECPF-ZEROS          PIC 9(003).
               10  BVVEGX-ECPF-NRO            PIC 9(009).
               10  BVVEGX-ECPF-CTRL           PIC 9(002).
           05  BVVEGX-TIPO-OPER               PIC X(001).
           05  BVVEGX-AGENCIA                 PIC 9(005)     COMP-3.
           05  BVVEGX-CONTA-NRO               PIC 9(013)     COMP-3.
           05  BVVEGX-CONTA-DIG               PIC X(001).
           05  BVVEGX-IND-TRAVA               PIC X(001).
           05  BVVEGX-SIT-PROCES              PIC X(001).
           05  BVVEGX-DTINIC-TRAVA            PIC 9(009)     COMP-3.
           05  BVVEGX-DTFIM-TRAVA             PIC 9(009)     COMP-3.
           05  BVVEGX-DTENVIO-BANCO           PIC 9(009)     COMP-3.
           05  BVVEGX-CNPJ-REDECARD.
               10  BVVEGX-RCNPJ-NRO           PIC 9(008).
               10  BVVEGX-RCNPJ-FILIAL        PIC 9(004).
               10  BVVEGX-RCNPJ-CTRL          PIC 9(002).
           05  BVVEGX-CPF-REDECARD REDEFINES BVVEGX-CNPJ-REDECARD.
               10  BVVEGX-RCPF-ZEROS          PIC 9(003).
               10  BVVEGX-RCPF-NRO            PIC 9(009).
               10  BVVEGX-RCPF-CTRL           PIC 9(002).
           05  BVVEGX-DTTRAVA-REDECARD        PIC 9(009)     COMP-3.
           05  BVVEGX-COD-RETORNO             PIC 9(005)     COMP-3.
           05  BVVEGX-MSG-ERRO                PIC X(060).
           05  BVVEGX-TPO-BANDEIRA            PIC X(002).
      *----------------------------------------------------------------*
      * 001-002 CODIGO DO BANCO QUE ESTA ENVIANDO ARQUIVOS
      * 003-007 DATA PROCESSAMENTO REDECARD (AAAAMMDD)
      * 008-011 HORA PROCESSAMENTO REDECARD (HHMMSS)
      * 012-016 VERSAO DO ARQUIVO DO BANCO
      * 017-021 VERSAO DO ARQUIVO DA REDECARD
      * 022-026 NUMERO DO PONTO DE VENDA=CODIGO DO ESTABELECIMENTO COML
      * 027-040 NUMERO DO CNPJ/CPF DO ESTABELECIMENTO
      * 041-041 TIPO DE OPERACAO C=CREDITO D=DEBITO
      * 042-044 AGENCIA DA CONTA CORRENTE DO ESTABELECIMENTO
      * 045-051 NUMERO DA CONTA CORRENTE DO ESTABELECIMENTO
      * 052-052 DIGITO DA CONTA CORRENTE DO ESTABELECIMENTO
      * 053-053 INDICADOR DE TRAVA T=TRAVA D=DESTRAVA
      * 054-054 STATUS DO PROCESSAMENTO P=PROCESSAMENTO A=AVISO
      * 055-059 DATA DE INICIO DE TRAVA SOLICITADO PELO BANCO AAAAMMDD
      * 060-064 DATA DE TERMINO DE TRAVA SOLICITADO PELO BANCO AAAAMMDD
      * 065-069 DATA DO REGISTRO ENVIADO PELO BANCO AAAAMMDD
      * 070-083 NUMERO DO CNPJ/CPF DO ESTABELECIMENTO NA REDECARD
      * 084-088 DATA DE FIM DE TRAVA ACATADA PELA REDECARD AAAAMMDD
      * 089-091 CODIGO DE ERRO OU AVISO DEPENDENDO DA SIT DO PROCESSAM.
      * 082-151 MENSAG DE ERRO OU AVISO DEPENDENDO DA SIT DO PROCESSAM.
      * 151-153 TIPO DE BANDEIRA: VISA OU MASTERCARD
      *----------------------------------------------------------------*

