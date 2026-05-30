      *----------------------------------------------------------------*
      *  SISTEMA : BVVE - CARTAO DE CREDITO / VENDEDOR                 *
      *  ARQUIVO : STATUS REDECARD PARA CARGA DAS TABELAS BVVEB023     *
      *            E BVVEB024                                          *
      *  NOME INC: I#BVVEH6                                            *
      *  DATA    : 13/08/2008                                          *
      *----------------------------------------------------------------*
      *
       01  BVVEH6-STATUS-REDECARD.
           05  FILLER                          PIC X(63)  VALUE
           'AN3PENDENTE DE ALCADA DE RISCO - CONTROLE CADASTRAL REDECARD
      -    '   '.
           05  FILLER                          PIC X(63)  VALUE
           'BN3BLOQUEADA POR CAMPANHA - VENCIDA/FORA DE PRAZO DE CARENCI
      -    'A  '.
           05  FILLER                          PIC X(63)  VALUE
           'CN3EM CADASTRAMENTO - JUNTO AO CONTROLE CADASTRAL
      -    '   '.
           05  FILLER                          PIC X(63)  VALUE
           'DN3PENDENTE DE DADOS - EM CREDENCIAMENTO NO CONTROLE CADASTR
      -    'AL '.
           05  FILLER                          PIC X(63)  VALUE
           'EN3REJEITADA POR DOCUMENTACAO COM PROBLEMAS APOS VERIFICACAO
      -    '   '.
           05  FILLER                          PIC X(63)  VALUE
           'FN4FILIADO - ESTABELECIMENTO CREDENCIADO JUNTO A REDECARD
      -    '   '.
           05  FILLER                          PIC X(63)  VALUE
           'GN3AGUARDANDO AVALIACAO TECNICA (DA EMPRESA TECNICA)
      -    '   '.
           05  FILLER                          PIC X(63)  VALUE
           'HN3ESTAB. FILIADO NO GE MAS HOUVE ERRO PARA ENCERRAMENTO
      -    '   '.
           05  FILLER                          PIC X(63)  VALUE
           'JN3REJEITADO PELA AVALIACAO TECNICA-EQUIPAMENTO NAO COMPATIV
      -    'EL '.
           05  FILLER                          PIC X(63)  VALUE
           'LN3PENDENTE DE CONTRATO SOCIAL - PENDENTE DE DOCUMENTACAO
      -    '   '.
           05  FILLER                          PIC X(63)  VALUE
           'MN3RECUSADO POR MATRIZ DE RISCO - APOS CONSULTA JUNTO AO SER
      -    'ASA'.
           05  FILLER                          PIC X(63)  VALUE
           'NN3PENDENTE DE NEGOCIACAO (NEGOCIACAO JUNTO AO CLIENTE)
      -    '   '.
           05  FILLER                          PIC X(63)  VALUE
           'ON3PENDENTE DE DOCUMENTACAO - PROPOSTA RECEPCIONADA NA REDEC
      -    'ARD'.
           05  FILLER                          PIC X(63)  VALUE
           'PN3PENDENTE DE MATRIZ DE RISCO - PENDENTE PESQUISA NO SERASA
      -    '   '.
           05  FILLER                          PIC X(63)  VALUE
           'RN3RECUSADO TARGET-REJEITADA POR RESTRICAO DO RAMO DE ATIVID
      -    'ADE'.
           05  FILLER                          PIC X(63)  VALUE
           'SN3PROPOSTA JA SE ENCONTRA EM CREDENCIAMENTO POR OUTRO CANAL
      -    '   '.
           05  FILLER                          PIC X(63)  VALUE
           'TS3RECUSADO PELO CONTROLE CADASTRAL - INCONSIST DE INFORMACO
      -    'ES '.
           05  FILLER                          PIC X(63)  VALUE
           'VN3PROPOSTA FOI APROVADA APOS STATUS "A"-SERA GERADO STATUS
      -    '"F"'.
           05  FILLER                          PIC X(63)  VALUE
           'XS3RECUSADO POR EXCECAO - FOI REJEITADA APOS STATUS "A"
      -    '   '.
           05  FILLER                          PIC X(63)  VALUE
           'ZS3RECUSADO CRITICA FISICA E/OU LOGICA CONFORME TABELA DE ER
      -    'ROS'.
       01  FILLER  REDEFINES  BVVEH6-STATUS-REDECARD.
           05  BVVEH6-CAMPOS        OCCURS        20.
               10  BVVEH6-STATUS-COD       PIC X(01).
               10  BVVEGA-ACEITA-CORRECAO  PIC X(01).
               10  BVVEGA-SIT-PROPOSTA     PIC 9(01).
               10  BVVEGA-STATUS-DESCR     PIC X(60).
