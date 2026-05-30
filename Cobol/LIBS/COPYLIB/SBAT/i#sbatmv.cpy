      *========================================================*
      *   I#SBATMV - MCA DA INTERFACE COM MQSERIES (SBAT3510)  *
      *========================================================*

       01  MQ-MCA.
      *    FORMATO DA AREA MCA
           05  MQ-FORMATO            PIC  9(08)  COMP.

      *    FUNCAO A SER EXECUTADA  ( OPEN/GET/PUT/CLOSE )
           05  MQ-FUNCAO             PIC  X(10).

      *    OPCAO DA FUNCAO  ( GET/PUT/UNICO/BROWSE/CURSOR/" " )
           05  MQ-OPCAO              PIC  X(10).

      *    CODIGO DE RETORNO (OK = 0, WARNING = 1, ERRO = 2 )
           05  MQ-COMPLETION-CODE    PIC  9(08)  COMP.

      *    EXPLICACAO DO RETORNO (RELACIONADO COM OS COD RETORNO 1 E 2)
           05  MQ-REASON-CODE        PIC  9(08)  COMP.

      *    HANDLE QUE IDENTIFICA A FILA SOBRE A QUAL INCIDIRA A FUNCAO
           05  MQ-HANDLE-FILA        PIC  9(08)  COMP.

      *    NOME DA FILA A SER ABERTA (USADO APENAS NA FUNCAO "OPEN")
           05  MQ-FILA               PIC  X(48).

      *    NOME DA FILA DE RESPOSTA
           05  MQ-FILA-RESPOSTA      PIC  X(48).

      *    TEMPO DE ESPERA (EM SEGUNDOS) DA CHEGADA DA MENSAGEM (GET)
           05  MQ-TEMPO-ESPERA       PIC  9(08)  COMP.

      *    PRIORIDADE DA MENSAGEM (DE ZERO A NOVE)
           05  MQ-PRIORIDADE-MSG     PIC  9(08)  COMP.

      *    FUNCAO "PUT" -> INDICA QUAIS NOTIFICACOES DE EVENTO
      *    QUER RECEBER  (EX.: CHEGADA DA MENSAGEM NA FILA DESTINO)
      *    FUNCAO "GET" -> NOTIFICACAO RECEBIDA
           05  MQ-NOTIFICACAO        PIC  9(08)  COMP.

      *    FUNCAO = PUT -> INDICA SE ESPERA OBTER, OU NAO, RESPOSTA DA
      *    APLICACAO QUE RECEBERA A MENSAGEM
      *    FUNCAO = GET -> INDICA SE EH UMA RESPOSTA DA APLICACAO QUE
      *    RECEBEU A MENSAGEM OU UMA NOTIFICACAO DE EVENTO OCORRIDO.
           05  MQ-TIPO-MENSAGEM      PIC  9(08)  COMP.

      *    STRING DE IDENTIFICACAO DA MENSAGEM
           05  MQ-ID-MENSAGEM        PIC  X(24).

      *    STRING DE IDENTIFICACAO DA MENSAGEM A QUAL
      *    ESTA MENSAGEM ESTA RELACIONADA
           05  MQ-ID-DE-CORRELACAO   PIC  X(24).

      *    SOMENTE PARA A FUNCAO "PUT"
      *    2 - INDICA QUE A MENSAGEM SERA LIBERADA APENAS NO SYNCPOINT
      *    4 - INDICA QUE A MENSAGEM SERA LIBERADA IMEDIATAMENTE
           05  MQ-OPCOES-DE-PUT      PIC  9(08)  COMP.

      *    SOMENTE PARA A FUNCAO "GET"
      *    0 - NAO CONVERTE
      *    16384 - CONVERTE
           05  MQ-OPCOES-DE-GET      PIC  9(08)  COMP.

      *    SOMENTE PARA A FUNCAO "PUT"
      *    'S' - MANTEM OS IDS INFORMADOS
      *    'N' - GERA NOVOS IDS
           05  MQ-FORCA-IDS          PIC  X(01).

      *---------------------------------------------------------*
      *  OS CAMPOS ABAIXO PERTENCEM APENAS AO FORMATO 2 DA MCA  *
      *---------------------------------------------------------*

      *    SOMENTE PARA A FUNCAO "PUT"
      *    INFORMA COMO DEVEM SER GERADOS OS CAMPOS MSG_ID E
      *    CORREL_ID DAS MENSAGENS DE RESPOSTA E REPORT (COA/COD...)
      *    000 - MOVER O MSG_ID DA MENSAGEM ORIGINAL PARA O CORREL_ID
      *    064 - MANTER O MESMO CORREL_ID DA MENSAGEM ORIGINAL
      *    128 - MANTER O MESMO MSG_ID DA MENSAGEM ORIGINAL
      *    192 - MANTER OS MESMOS MSG_ID E CORREL_ID DA MSG ORIGINAL
           05  MQ-RETORNO-DE-IDS     PIC  9(03).

      *    SOMENTE PARA A FUNCAO "GET"
      *    SELECAO DE MENSAGENS COM RELACAO AO MQ-ID-MENSAGEM
      *    E AO MQ-ID-DE-CORRELACAO
      *    00 - NENHUM CRITERIO DE SELECAO
      *    01 - MSG_ID IGUAL
      *    02 - CORREL_ID IGUAL
      *    03 - MSG_ID E CORREL_ID IGUAIS
           05  MQ-CRITERIOS-GET      PIC  9(02).

      *---------------------------------------------------------*
      *  OS CAMPOS ABAIXO PERTENCEM APENAS AO FORMATO 3 DA MCA  *
      *---------------------------------------------------------*

      *    SOMENTE PARA A FUNCAO "GET"
      *    INFORMA A QUANTIDADE DE VEZES QUE ESTA MENSAGEM
      *    VOLTOU PARA A FILA.
           05  MQ-CONTA-BACKOUT      PIC  9(03) COMP-3.

      *    -> PARA A FUNCAO "GET", RETORNA O TEMPO DE VALIDADE
      *       RESTANTE DESTA MENSAGEM, EM DECIMOS DE SEGUNDO.
      *    -> PARA A FUNCAO "PUT", DEFINE O TEMPO DE VALIDADE
      *       DA MENSAGEM, EM DECIMOS DE SEGUNDO.
           05  MQ-EXPIRACAO-MSG      PIC  S9(08) COMP.

      *---------------------------------------------------------*
      *  OS CAMPOS ABAIXO PERTENCEM APENAS AO FORMATO 4 DA MCA  *
      *---------------------------------------------------------*

      *    INFORMA O REPLY TO QUEUE MANAGER A SER UTILIZADO
           05  MQ-QM-RESPOSTA        PIC  X(16).

      *    INFORMA O FORMATO DA MENSAGEM
           05  MQ-FORMATO-MSG        PIC  X(08).
      *
      *    RESERVA
           05  FILLER                PIC  X(12).
