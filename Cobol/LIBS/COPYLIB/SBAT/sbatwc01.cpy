      *===============================================================*
      *  COPYBOOK : SBATWC01 (COBOL)                                  *
      *  PROGRAMA : SBAT3520 -  EXECUTA COMANDOS DE MQSERIES          *
      *  DESCRICAO: AREA DE COMUNICACAO COM O PROGRAMA.               *
      *                                                               *
      *  CODIGOS DE RETORNO DO MQSERIES:                              *
      *  ------------------------------                               *
      *  > SUCESSO                                                    *
      *    MQ-COMPLETION-CODE: 0 (MQCC_OK)                            *
      *    MQ-REASON-CODE    : 0                                      *
      *                                                               *
      *  > ADVERTENCIA (WARNING)                                      *
      *    MQ-COMPLETION-CODE: 1 (MQCC_WARNING)                       *
      *    MQ-REASON-CODE    : VIDE MANUAL MQSERIES                   *
      *                                                               *
      *  > ERRO                                                       *
      *    MQ-COMPLETION-CODE: 2 (MQCC_FAILED)                        *
      *    MQ-REASON-CODE    : VIDE MANUAL MQSERIES                   *
      *                                                               *
      *  CODIGOS DE RETORNO DE CONSISTENCIA DOS CAMPOS PASSADOS:      *
      *  ------------------------------------------------------       *
      *  > MQ-FUNCAO INVALIDA                                         *
      *    MQ-COMPLETION-CODE:    2                                   *
      *    MQ-REASON-CODE    :-3500                                   *
      *                                                               *
      *  > MQ-FUNCAO ='OPEN' E  MQ-OPCAO = VALOR INVALIDO             *
      *    MQ-COMPLETION-CODE:    2                                   *
      *    MQ-REASON-CODE    :-3501                                   *
      *                                                               *
      *  > MQ-FUNCAO ='GET'  E  MQ-OPCAO = VALOR INVALIDO             *
      *    MQ-COMPLETION-CODE:    2                                   *
      *    MQ-REASON-CODE    :-3503                                   *
      *                                                               *
      *  > MQ-FUNCAO ='PUT'  E  MQ-OPCAO = VALOR INVALIDO             *
      *    MQ-COMPLETION-CODE:    2                                   *
      *    MQ-REASON-CODE    :-3504                                   *
      *                                                               *
      *  > MQ-TAMANHO-BUFFER = TAMANHO  DO BUFFER ZERADO.             *
      *    MQ-COMPLETION-CODE:    2                                   *
      *    MQ-REASON-CODE    :-3506                                   *
      *                                                               *
      *  > MQ-PTR-BUFFER-COMP= ENDERECO DO BUFFER ZERADO.             *
      *    MQ-COMPLETION-CODE:    2                                   *
      *    MQ-REASON-CODE    :-3507                                   *
      *                                                               *
      *  > MQ-PTR-BUFFER-COMP= NAO ENCONTROU LITERAL "SBAT3520" NO    *
      *                        INICIO DO BUFFER                       *
      *    MQ-COMPLETION-CODE:    2                                   *
      *    MQ-REASON-CODE    :-3508                                   *
      *                                                               *
      *  > MQ-PTR-BUFFER-COMP= NAO ENCONTROU LITERAL "SBAT3520" NO    *
      *                        FINAL DO BUFFER                        *
      *    MQ-COMPLETION-CODE:    2                                   *
      *    MQ-REASON-CODE    :-3509                                   *
      *                                                               *
      *  > MQ-PTR-BUFFER-COMP= FORMATO DO LAYOUT DA MCA EH INVALIDO.  *
      *    MQ-COMPLETION-CODE:    2                                   *
      *    MQ-REASON-CODE    :-3510                                   *
      *                                                               *
      *===============================================================*

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

      *    NOME DO QMANAGER A SER CONECTADO
      *    QUANDO BRANCO/LOW-VALUES, ASSUMI O QMANAGER DEFAULT
           05  MQ-QM-CONEXAO         PIC  X(48).

      *    NOME DA FILA DE RESPOSTA
           05  MQ-FILA-RESPOSTA      PIC  X(48).

      *    INFORMA O QUEUE MANAGER DE RESPOSTA (REPLY) A SER UTILIZADO
           05  MQ-QM-RESPOSTA        PIC  X(48).

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

      *    SOMENTE PARA A FUNCAO "GET"
      *    INFORMA A QUANTIDADE DE VEZES QUE ESTA MENSAGEM
      *    VOLTOU PARA A FILA.
           05  MQ-CONTA-BACKOUT      PIC  9(03) COMP-3.

      *    -> PARA A FUNCAO "GET", RETORNA O TEMPO DE VALIDADE
      *       RESTANTE DESTA MENSAGEM, EM DECIMOS DE SEGUNDO.
      *    -> PARA A FUNCAO "PUT", DEFINE O TEMPO DE VALIDADE
      *       DA MENSAGEM, EM DECIMOS DE SEGUNDO.
           05  MQ-EXPIRACAO-MSG      PIC  S9(08) COMP.

      *    INFORMA O FORMATO DA MENSAGEM
           05  MQ-FORMATO-MSG        PIC  X(08).
      *
      *    TAMANHO DO BUFFER
           05 MQ-TAMANHO-BUFFER      PIC  9(09) COMP.
      *
      *    PONTEIRO DO BUFFER A SER UTILIZADO PARA LEITURA OU GRAVACAO
           05  MQ-PTR-BUFFER         USAGE IS POINTER VALUE NULL.
           05  FILLER                REDEFINES MQ-PTR-BUFFER.
               07 MQ-PTR-BUFFER-COMP PIC  9(09) COMP.
      *    TAMANHO DO BUFFER
           05 MQ-TAMANHO-BUFFER-LIDO PIC  9(09) COMP.

      *    DATA DA INCLUSAO DA MENSAGEM NA FILA (FORMATO: AAAAMMDD)
           05  MQ-DATA-PUT           PIC  X(08).

      *    HORARIO DA INCLUSAO DA MENSAGEM NA FILA (FORMATO: HHMMSSDC)
           05  MQ-HORARIO-PUT        PIC  X(08).

      *    RESERVA
           05  FILLER                PIC  X(34).
