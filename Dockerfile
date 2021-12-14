FROM erlang:alpine

ADD _build/default/bin/ct_containers_ryuk /opt/ryuk
RUN chmod +x /opt/ryuk

EXPOSE 8999

CMD ["./opt/ryuk"]