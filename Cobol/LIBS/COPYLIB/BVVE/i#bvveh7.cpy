      *----------------------------------------------------------------*
      *  SISTEMA : BVVE - CARTAO DE CREDITO / VENDEDOR                 *
      *  ARQUIVO : SOLICITACOES DE TRAVA/DESTRAVA DE DOMICILIO         *
      *            REJEITADAS PELA REDECARD                            *
      *  LRECL   : 184 (FB)                                            *
      *  NOME INC: I#BVVEH7                                            *
      *  DATA    : 03/09/2008                                          *
      *----------------------------------------------------------------*
       01  BVVEH7-REG.
           05  BVVEH7-DATA-PROCES             PIC 9(009)     COMP-3.
           05  BVVEH7-HORA-PROCES             PIC 9(007)     COMP-3.
           05  BVVEH7-VRSARQ-BANCO            PIC 9(009)     COMP-3.
           05  BVVEH7-VRSARQ-REDECARD         PIC 9(009)     COMP-3.
           05  BVVEH7-PONTO-VENDA             PIC 9(009)     COMP-3.
           05  BVVEH7-TIPO-PESSOA             PIC X(001).
           05  BVVEH7-CNPJ-ESTAB.
               10  BVVEH7-ECNPJ-NRO           PIC 9(008).
               10  BVVEH7-ECNPJ-FILIAL        PIC 9(004).
               10  BVVEH7-ECNPJ-CTRL          PIC 9(002).
           05  BVVEH7-CPF-ESTAB REDEFINES BVVEH7-CNPJ-ESTAB.
               10  BVVEH7-ECPF-ZEROS          PIC 9(003).
               10  BVVEH7-ECPF-NRO            PIC 9(009).
               10  BVVEH7-ECPF-CTRL           PIC 9(002).
           05  BVVEH7-TIPO-OPER               PIC X(001).
           05  BVVEH7-BANCO                   PIC 9(003)     COMP-3.
           05  BVVEH7-AGENCIA                 PIC 9(005)     COMP-3.
           05  BVVEH7-CONTA-NRO               PIC 9(013)     COMP-3.
           05  BVVEH7-CONTA-DIG               PIC X(001).
           05  BVVEH7-IND-TRAVA               PIC X(001).
           05  BVVEH7-SIT-PROCES              PIC X(001).
           05  BVVEH7-DTINIC-TRAVA            PIC 9(009)     COMP-3.
           05  BVVEH7-DTFIM-TRAVA             PIC 9(009)     COMP-3.
           05  BVVEH7-DTENVIO-BANCO           PIC 9(009)     COMP-3.
           05  BVVEH7-CNPJ-REDECARD.
               10  BVVEH7-RCNPJ-NRO           PIC 9(008).
               10  BVVEH7-RCNPJ-FILIAL        PIC 9(004).
               10  BVVEH7-RCNPJ-CTRL          PIC 9(002).
           05  BVVEH7-CPF-REDECARD REDEFINES BVVEH7-CNPJ-REDECARD.
               10  BVVEH7-RCPF-ZEROS          PIC 9(003).
               10  BVVEH7-RCPF-NRO            PIC 9(009).
               10  BVVEH7-RCPF-CTRL           PIC 9(002).
           05  BVVEH7-DTTRAVA-REDECARD        PIC 9(009)     COMP-3.
           05  BVVEH7-COD-RETORNO             PIC 9(005)     COMP-3.
           05  BVVEH7-MSG-ERRO                PIC X(060).
           05  BVVEH7-CEMPR-INC               PIC 9(05)      COMP-3.
           05  BVVEH7-CDEPDC                  PIC 9(05)      COMP-3.
           05  BVVEH7-ERRO-BRADESCO           PIC X(24).
           05  BVVEH7-TPO-BANDEIRA            PIC X(002).
      *----------------------------------------------------------------*
      * 001-005 DATA PROCESSAMENTO/ENVIO ARQ PELA REDECARD (AAAAMMDD)
      * 006-009 HORARIO DO PROCESSAMENTO NA REDECARD
      * 010-014 VERSAO DO ARQUIVO ENVIADO PELO BANCO
      * 015-019 VERSAO DO ARQUIVO ENVIADO PELA REDECARD
      * 020-024 NUMERO DO PONTO DE VENDA=CODIGO DO ESTABELECIMENTO COML
      * 025-025 TIPO DE PESSOA ('F'=FISICA 'J'=JURIDA '?'=INDETERMINADO)
      * 026-039 NUMERO DO CNPJ/CPF DO ESTABELECIMENTO
      * 040-040 TIPO DE OPERACAO C=CREDITO D=DEBITO
      * 041-042 BANCO DA CONTA CORRENTE DO ESTABELECIMENTO
      * 043-045 AGENCIA DA CONTA CORRENTE DO ESTABELECIMENTO
      * 046-052 NUMERO DA CONTA CORRENTE DO ESTABELECIMENTO
      * 053-053 DIGITO DA CONTA CORRENTE DO ESTABELECIMENTO
      * 054-054 INDICADOR DE TRAVA T=TRAVA D=DESTRAVA
      * 055-055 STATUS DO PROCESSAMENTO P=PROCESSAMENTO A=AVISO
      * 056-060 DATA DE INICIO DE TRAVA SOLICITADO PELO BANCO AAAAMMDD
      * 061-065 DATA DE TERMINO DE TRAVA SOLICITADO PELO BANCO AAAAMMDD
      * 066-070 DATA DE ENVIO DO ARQ/SOLICITACAO PELO BANCO AAAAMMDD
      * 071-084 NUMERO DO CNPJ/CPF DO ESTABELECIMENTO NA REDECARD
      * 085-089 DATA DE FIM DE TRAVA ACATADA PELA REDECARD AAAAMMDD
      * 090-092 CODIGO DE ERRO OU AVISO DEPENDENDO DA SIT DO PROCESSAM.
      * 093-152 MENSAG DE ERRO OU AVISO DEPENDENDO DA SIT DO PROCESSAM.
      * 153-155 CODIGO DA EMPRESA/BANCO INCORPORADA(O)
      * 156-158 CODIGO DA DEPENDENCIA BRADESCO (AGENCIA/DEPTO)
      * 159-182 ERRO DETECTADO PELO BRADESCO
      * 183-184 TIPO DE BANDEIRA: VISA OU MASTERCARD
      *----------------------------------------------------------------*
